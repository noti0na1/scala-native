package scala.scalanative.sbtplugin.snc

import java.net.URI
import java.nio.file.{Files, Path, Paths}
import java.util.EnumSet

import scala.collection.mutable
import scala.util.control.NonFatal

import xsbti.api.DependencyContext
import xsbti.{AnalysisCallback, Logger, UseScope, VirtualFile}

/** Forwards the analysis events of `snc --events --analysis` (events protocol
 *  1.1, described in the README of the companion Scala 3 fork) to Zinc's
 *  callback, one event to one call, in the order `snc` sent them. Sources are
 *  ordinals into the compiled sources. Product paths point into the staging
 *  directory and are relocated to the output directory, where the files have
 *  been moved.
 *
 *  Files that no event names (TASTy, and NIR classes that only the Native
 *  backend generates, such as lambdas) become local products of the source of
 *  the nearest class they are named after ([[SncAnalysis.owners]]).
 *
 *  A dependency on a class of the JDK (`"jdk": true`, an archive of the JDK
 *  that `snc` read) reaches Zinc with [[jdkClassFile]] as its class file.
 */
private[scalanative] object SncAnalysisEvents {
  import SncJson.Fields

  /** The class file of the JDK's classes as `scalac` reports it on this JVM: a
   *  path in the `jrt:` file system, or `rt.jar` on Java 8. Zinc recognizes
   *  both: it records the dependency only when a source of the project defines
   *  the class, and otherwise does not track it. `snc` reports the archive
   *  files it read instead (`lib/ct.sym`, or a jmod in `jmods/`), which Zinc
   *  would take for libraries on the class path: whenever the class path
   *  changes, Zinc would look the class up, find it in a Scala Native library
   *  that has its NIR rather than in the archive, and recompile every source
   *  that uses the JDK.
   */
  private lazy val jdkClassFile: Path =
    try Paths.get(URI.create("jrt:/"))
    catch {
      case NonFatal(_) => Paths.get(sys.props("java.home"), "lib", "rt.jar")
    }

  def forward(
      events: Seq[Any],
      sources: IndexedSeq[VirtualFile],
      relocate: Path => Path,
      outputDirectory: Path,
      writtenFiles: Seq[Path],
      callback: AnalysisCallback,
      log: Logger
  ): Unit = {
    val named = mutable.Set.empty[Path]
    val sourceOfClass = mutable.Map.empty[String, VirtualFile]
    def source(e: Any): VirtualFile =
      sources(e.int("source").getOrElse(sys.error(s"no source in $e")))
    def string(e: Any, name: String): String =
      e.string(name).getOrElse(sys.error(s"no $name in $e"))
    def product(e: Any): Option[Path] = {
      val file = relocate(Paths.get(string(e, "file")))
      // Zinc treats a product that does not exist as removed, and would
      // recompile its source every time.
      if (Files.isRegularFile(file)) { named += file; Some(file) }
      else {
        log.debug(() => s"snc reported $file, which it did not write")
        None
      }
    }

    events.foreach { e =>
      e.string("event") match {
        case Some("startSource") => callback.startSource(source(e))
        case Some("api")         =>
          callback.api(source(e), SncApiJson.classLike(e.field("classLike")))
        case Some("mainClass") =>
          callback.mainClass(source(e), string(e, "name"))
        case Some("usedName") =>
          val scopes = EnumSet.noneOf(classOf[UseScope])
          e.array("scopes")
            .foreach(s => scopes.add(UseScope.valueOf(s.toString)))
          callback.usedName(string(e, "className"), string(e, "name"), scopes)
        case Some("binaryDependency") =>
          callback.binaryDependency(
            if (e.field("jdk") == true) jdkClassFile
            else Paths.get(string(e, "entry")),
            string(e, "binaryName"),
            string(e, "fromClass"),
            source(e),
            DependencyContext.valueOf(string(e, "context"))
          )
        case Some("classDependency") =>
          callback.classDependency(
            string(e, "on"),
            string(e, "from"),
            DependencyContext.valueOf(string(e, "context"))
          )
        case Some("generatedNonLocalClass") =>
          product(e).foreach { file =>
            val binaryName = string(e, "binaryName")
            sourceOfClass(binaryName) = source(e)
            callback.generatedNonLocalClass(
              source(e),
              file,
              binaryName,
              string(e, "srcName")
            )
          }
        case Some("generatedLocalClass") =>
          product(e).foreach(callback.generatedLocalClass(source(e), _))
        case Some("apiPhaseCompleted")        => callback.apiPhaseCompleted()
        case Some("dependencyPhaseCompleted") =>
          callback.dependencyPhaseCompleted()
        case _ => () // start, diagnostic and done are read elsewhere
      }
    }

    writtenFiles.filterNot(named).foreach { file =>
      val relative = outputDirectory.relativize(file).toString
      val stem = relative.substring(0, relative.lastIndexOf('.'))
      SncAnalysis
        .owners(stem.replace(java.io.File.separatorChar, '.'))
        .flatMap(sourceOfClass.get)
        .headOption match {
        case Some(source) => callback.generatedLocalClass(source, file)
        case None         =>
          log.debug(() => s"snc wrote $file for no known source")
      }
    }
  }
}
