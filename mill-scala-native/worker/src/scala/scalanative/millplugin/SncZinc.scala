package scala.scalanative.millplugin

import sbt.internal.inc.consistent.ConsistentFileAnalysisStore
import sbt.internal.inc.{
  Analysis, FreshCompilerCache, IncrementalCompilerImpl, Locate,
  MappedFileConverter, ScalaInstance, Stamps, ZincUtil
}

import java.io.File
import java.nio.file.{Files, Path}
import java.util.Optional
import java.util.function.Supplier

import scala.collection.mutable
import scala.util.control.NonFatal

import scala.scalanative.sbtplugin.snc.SncCompiler

import xsbti.compile.analysis.ReadWriteMappers
import xsbti.compile.{
  AnalysisContents, AnalysisStore, ClasspathOptions, CompileAnalysis,
  CompileOrder, DefinesClass, IncOptions, MiniSetup, PerClasspathEntryLookup,
  PreviousResult
}
import xsbti.{PathBasedFile, Position, Problem, Reporter, Severity, T2}

/** Zinc's incremental compiler with `snc` as its Scala compiler, for Mill.
 *
 *  This is what Mill's own Zinc worker does for `scalac`, with the sbt plugin's
 *  [[SncCompiler]] in place of the compiler bridge: Mill's worker builds its
 *  compiler from a Scala instance and a bridge jar, and takes no compiler
 *  object. The analysis is stored as Mill stores it, with the Zinc version Mill
 *  uses, so that Mill's own Zinc compiles of dependent modules and its test
 *  discovery read it.
 */
private[millplugin] object SncZinc {

  /** Where messages, problems and summaries go: the plugin, which prints them
   *  as Mill prints those of its own Zinc compiles and passes the problems on
   *  to the task's reporter.
   */
  trait Sink {
    def debugEnabled: Boolean
    def log(level: String, message: String): Unit
    def problem(problem: Problem): Unit
    def summary(): Unit
  }

  final case class Request(
      snc: Path,
      sncOptions: Seq[String],
      workingDirectory: Path,
      scalaVersion: String,
      sources: Seq[Path],
      classpath: Seq[Path],
      options: Seq[String],
      classesDirectory: Path,
      analysisFile: Path,
      upstreamAnalyses: Map[Path, Path],
      incremental: Boolean,
      reportCachedProblems: Boolean
  )

  private val compiler = new IncrementalCompilerImpl

  /** Compiles; `None` on success, else why it failed. */
  def compile(request: Request, sink: Sink): Option[String] = {
    import request._
    if (!incremental) delete(classesDirectory)
    Files.createDirectories(classesDirectory)

    val converter = MappedFileConverter.empty
    val logger = new SinkLogger(sink)
    val reporter = new SinkReporter(sink)
    val store = analysisStore(analysisFile)

    val definesClassIn =
      SncCompiler.definesClass(Locate.definesClass(_), converter.toPath(_))
    val lookup = new PerClasspathEntryLookup {
      override def analysis(
          entry: xsbti.VirtualFile
      ): Optional[CompileAnalysis] =
        entry match {
          case file: PathBasedFile =>
            upstreamAnalyses.get(normalize(file.toPath)) match {
              case Some(analysis) if Files.isRegularFile(analysis) =>
                analysisStore(analysis).get().map(_.getAnalysis)
              case _ => Optional.empty[CompileAnalysis]()
            }
          case _ => Optional.empty[CompileAnalysis]()
        }
      override def definesClass(entry: xsbti.VirtualFile): DefinesClass =
        definesClassIn(entry)
    }

    // A Scala instance with only a version: Zinc records the version in the
    // setup, and with these class path options it adds no compiler jars.
    val classpathOptions =
      ClasspathOptions.of(false, false, false, false, false)
    val instance = new ScalaInstance(
      scalaVersion,
      null,
      null,
      null,
      Array.empty[File],
      Array.empty[File],
      Array.empty[File],
      Some(scalaVersion)
    )
    val scalac = new SncCompiler(
      snc,
      sncOptions,
      instance,
      classpathOptions,
      workingDirectory.toFile
    )
    val compilers = ZincUtil.compilers(instance, classpathOptions, None, scalac)

    // As in the sbt plugin: Zinc compares the setup's extra, so a different
    // `snc` binary or different `snc` options recompile everything. An `snc`
    // without the analysis reports no dependencies, so Zinc recompiles
    // everything whenever a source or the class path changes.
    val reportsAnalysis = SncCompiler.reportsAnalysis(snc.toFile)
    val extra: Array[T2[String, String]] = (Seq(
      pair("scalanative.snc", SncCompiler.identity(snc.toFile)),
      pair("scalanative.snc.options", sncOptions.mkString(" "))
    ) ++ (
      if (reportsAnalysis) Nil
      else
        Seq(
          pair(
            "scalanative.snc.classpath",
            SncCompiler.classpathDigest(classpath)
          )
        )
    )).toArray
    // As in Mill, `-color:` options do not invalidate the analysis.
    val incOptions = {
      val options =
        IncOptions.of().withIgnoredScalacOptions(Array("-color:.*"))
      if (reportsAnalysis) options else options.withRecompileAllFraction(0.0)
    }
    val setup = compiler.setup(
      lookup = lookup,
      skip = false,
      cacheFile = analysisFile,
      cache = new FreshCompilerCache,
      incOptions = incOptions,
      reporter = reporter,
      progress = None,
      earlyAnalysisStore = None,
      extra = extra
    )
    val previous =
      if (!incremental) compiler.emptyPreviousResult
      else {
        val contents = store.get()
        PreviousResult.of(
          contents.map[CompileAnalysis](_.getAnalysis),
          contents.map[MiniSetup](_.getMiniSetup)
        )
      }
    val inputs = compiler.inputs(
      classpath = (classpath :+ classesDirectory)
        .map(converter.toVirtualFile(_))
        .toArray,
      sources = sources.map(converter.toVirtualFile(_)).toArray,
      classesDirectory = classesDirectory,
      earlyJarPath = None,
      scalacOptions = options.toArray,
      javacOptions = Array.empty[String],
      maxErrors = 100,
      sourcePositionMappers = Array.empty,
      order = CompileOrder.Mixed,
      compilers = compilers,
      setup = setup,
      pr = previous,
      temporaryClassesDirectory = Optional.empty[Path](),
      converter = converter,
      stampReader = Stamps.timeWrapBinaryStamps(converter)
    )
    try {
      val result = compiler.compile(inputs, logger)
      if (reportCachedProblems) reporter.logOldProblems(result.analysis)
      store.set(AnalysisContents.create(result.analysis, result.setup))
      None
    } catch {
      case e: xsbti.CompileFailed => Some(e.toString)
    }
  }

  /** The main classes that `analysisFile` records, sorted. */
  def mainClasses(analysisFile: Path): Seq[String] =
    if (!Files.isRegularFile(analysisFile)) Nil
    else {
      val contents = analysisStore(analysisFile).get()
      if (!contents.isPresent) Nil
      else
        contents.get.getAnalysis match {
          case a: Analysis =>
            a.infos.allInfos.values
              .flatMap(_.getMainClasses.toSeq)
              .toSeq
              .distinct
              .sorted
          case _ => Nil
        }
    }

  /** As Mill's Zinc worker stores analyses. */
  private def analysisStore(file: Path): AnalysisStore =
    ConsistentFileAnalysisStore.binary(
      file = file.toFile,
      mappers = ReadWriteMappers.getEmptyMappers,
      reproducible = true,
      parallelism = math.min(Runtime.getRuntime.availableProcessors(), 8)
    )

  private def normalize(path: Path): Path = path.toAbsolutePath.normalize

  private def pair(key: String, value: String): T2[String, String] =
    new T2[String, String] {
      override def get1(): String = key
      override def get2(): String = value
    }

  private def delete(path: Path): Unit =
    if (Files.exists(path)) {
      val all = mutable.ArrayBuffer.empty[Path]
      val files = Files.walk(path)
      try files.forEach(p => all += p)
      finally files.close()
      all.reverseIterator.foreach(Files.delete)
    }

  private final class SinkLogger(sink: Sink) extends xsbti.Logger {
    override def error(msg: Supplier[String]): Unit = sink.log("error", msg.get)
    override def warn(msg: Supplier[String]): Unit = sink.log("warn", msg.get)
    override def info(msg: Supplier[String]): Unit = sink.log("info", msg.get)
    override def debug(msg: Supplier[String]): Unit =
      if (sink.debugEnabled) sink.log("debug", msg.get)
    override def trace(exception: Supplier[Throwable]): Unit =
      if (sink.debugEnabled) {
        val out = new java.io.StringWriter
        exception.get.printStackTrace(new java.io.PrintWriter(out))
        sink.log("debug", out.toString)
      }
  }

  /** Keeps the problems for Zinc and passes them and the summaries on. */
  private final class SinkReporter(sink: Sink) extends Reporter {
    private val logged = mutable.ArrayBuffer.empty[Problem]
    private val seen = mutable.Set.empty[Seq[Any]]

    private def key(p: Problem): Seq[Any] = {
      val pos = p.position
      Seq(
        p.severity,
        p.message,
        pos.sourcePath.orElse(""),
        pos.line.orElse(-1),
        pos.pointer.orElse(-1)
      )
    }

    override def reset(): Unit = {
      logged.clear(); seen.clear()
    }
    override def hasErrors(): Boolean =
      logged.exists(_.severity == Severity.Error)
    override def hasWarnings(): Boolean =
      logged.exists(_.severity == Severity.Warn)
    override def problems(): Array[Problem] = logged.toArray
    override def comment(pos: Position, msg: String): Unit = ()

    override def log(problem: Problem): Unit = {
      logged += problem
      seen += key(problem)
      try sink.problem(problem)
      catch { case NonFatal(e) => sink.log("debug", s"reporter: $e") }
    }

    override def printSummary(): Unit = sink.summary()

    /** Problems of sources that were not recompiled, from the analysis (Mill's
     *  `zincReportCachedProblems`).
     */
    def logOldProblems(analysis: CompileAnalysis): Unit = analysis match {
      case a: Analysis =>
        val old = a.infos.allInfos.values
          .flatMap(i => i.getReportedProblems ++ i.getUnreportedProblems)
          .filterNot(p => seen(key(p)))
          .toSeq
        if (old.nonEmpty) {
          old.foreach(log)
          printSummary()
        }
      case _ => ()
    }
  }
}
