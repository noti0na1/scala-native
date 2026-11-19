package scala.scalanative.sbtplugin.snc

import java.io.File
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest
import java.util.Optional

import scala.collection.mutable.ListBuffer

import xsbti.compile.{
  ClasspathOptions, CompileProgress, Compilers, DefinesClass, DependencyChanges,
  Output, ScalaCompiler, ScalaInstance
}
import xsbti.{
  AnalysisCallback, AnalysisCallback2, FileConverter, Logger, Problem, Reporter,
  Severity, VirtualFile
}

/** Zinc's Scala compiler, implemented by running `snc`, the native Scala 3
 *  compiler, in a subprocess.
 *
 *  `snc` writes TASTy and NIR, and no class files. It compiles into a staging
 *  directory; when it succeeds, its files move into the output directory and
 *  are reported to Zinc, by `snc`'s analysis events ([[SncAnalysisEvents]]) or,
 *  without them, by [[SncAnalysis]]. Problems go to Zinc's reporter.
 *
 *  `instance` gives the Scala version that Zinc records. In sbt it is the JVM
 *  compiler's instance, which `console` and `doc` use.
 */
private[scalanative] final class SncCompiler(
    snc: Path,
    extraOptions: Seq[String],
    instance: ScalaInstance,
    cpOptions: ClasspathOptions,
    workingDirectory: File
) extends ScalaCompiler {

  override def scalaInstance(): ScalaInstance = instance

  override def classpathOptions(): ClasspathOptions = cpOptions

  override def compile(
      sources: Array[VirtualFile],
      classpath: Array[VirtualFile],
      converter: FileConverter,
      changes: DependencyChanges,
      options: Array[String],
      output: Output,
      callback: AnalysisCallback,
      reporter: Reporter,
      progress: Optional[CompileProgress],
      log: Logger
  ): Unit = {
    val outputDirectory = SncCompiler.outputDirectory(output)
    val staging = outputDirectory.resolveSibling(
      outputDirectory.getFileName.toString + "-snc"
    )
    SncCompiler.delete(staging)
    Files.createDirectories(staging)

    val sourcePaths = sources.toSeq.map(s => converter.toPath(s).toAbsolutePath)
    val classpathPaths =
      classpath.toSeq.map(c => converter.toPath(c).toAbsolutePath)
    val about = SncCommand.about(snc)
    val allOptions = options.toSeq ++ extraOptions
    val arguments = SncCommand.arguments(
      about = about,
      options = allOptions,
      classpath = classpathPaths,
      outputDirectory = staging,
      sources = sourcePaths
    )
    val argumentFile =
      staging.resolveSibling(staging.getFileName.toString + ".args")
    log.debug(() => "Running " + SncCommand.show(snc, arguments))
    val start = System.nanoTime()
    val result = SncCommand.run(
      snc,
      arguments,
      argumentFile,
      workingDirectory,
      line => log.debug(() => line)
    )
    val elapsed = (System.nanoTime() - start) / 1000000L

    // With events, other output is on stderr: echoed messages, or a crash.
    val ids = sources.toSeq
      .zip(sourcePaths)
      .map { case (source, path) => path.toString -> source.id }
      .toMap
    val sourceId = (path: String) => ids.getOrElse(path, path)
    val (problems, otherLines) =
      if (about.exists(_.supportsEvents))
        (SncDiagnostics.fromEvents(result.events, sourceId), result.output)
      else {
        val parsed = SncDiagnostics.parse(result.output, sourceId)
        (parsed.problems, parsed.otherLines)
      }
    problems.foreach { problem =>
      reporter.log(problem)
      callback match {
        case callback: AnalysisCallback2 =>
          callback.problem2(
            problem.category,
            problem.position,
            problem.message,
            problem.severity,
            true,
            problem.rendered,
            problem.diagnosticCode,
            problem.diagnosticRelatedInformation,
            problem.actions
          )
        case _ =>
          callback.problem(
            problem.category,
            problem.position,
            problem.message,
            problem.severity,
            true
          )
      }
    }
    val failed =
      result.exitCode != 0 || problems.exists(_.severity == Severity.Error)
    otherLines.filter(_.trim.nonEmpty).foreach { line =>
      if (failed) log.error(() => line) else log.warn(() => line)
    }
    reporter.printSummary()
    if (failed) {
      log.debug(() => s"snc exited with ${result.exitCode} after $elapsed ms")
      if (result.exitCode >= 2)
        log.error(() =>
          s"snc exited with ${result.exitCode}: " +
            SncCommand.show(
              snc,
              SncCommand.commandArguments(arguments, argumentFile)
            )
        )
      SncCompiler.delete(staging)
      throw new SncCompileFailed(arguments.toArray, problems.toArray)
    }
    val analysisStart = System.nanoTime()
    val products = SncCompiler.moveTree(staging, outputDirectory)
    SncCompiler.delete(staging)
    if (about.exists(_.supportsAnalysis))
      SncAnalysisEvents.forward(
        events = result.events,
        sources = sources.toIndexedSeq,
        relocate = path =>
          if (path.startsWith(staging))
            outputDirectory.resolve(staging.relativize(path).toString)
          else path,
        outputDirectory = outputDirectory,
        writtenFiles = products,
        callback = callback,
        log = log
      )
    else
      SncAnalysis.report(
        sources = sources.toSeq.zip(sourcePaths),
        products = products,
        outputDirectory = outputDirectory,
        classpath = classpathPaths,
        workingDirectory = workingDirectory.toPath.toAbsolutePath,
        callback = callback,
        log = log
      )
    val analysisElapsed = (System.nanoTime() - analysisStart) / 1000000L
    log.debug(() =>
      s"snc compiled ${sources.length} sources in $elapsed ms; " +
        s"${products.size} products reported in $analysisElapsed ms"
    )
  }
}

private[scalanative] object SncCompiler {

  /** `default`, with its Scala compiler replaced by `snc`. */
  def compilers(
      snc: File,
      extraOptions: Seq[String],
      default: Compilers,
      workingDirectory: File
  ): Compilers = {
    val scalac = default.scalac
    Compilers.of(
      new SncCompiler(
        snc.toPath.toAbsolutePath,
        extraOptions,
        scalac.scalaInstance,
        scalac.classpathOptions,
        workingDirectory
      ),
      default.javaTools
    )
  }

  /** Whether `snc` streams Zinc's analysis, so that Zinc can compile
   *  incrementally.
   */
  def reportsAnalysis(snc: File): Boolean =
    SncCommand.about(snc.toPath.toAbsolutePath).exists(_.supportsAnalysis)

  /** `tool` as given, or for a bare name that is not a file in the working
   *  directory, the executable of that name on the `PATH`.
   */
  def onPath(tool: File): File =
    if (tool.isFile || tool.getParentFile != null) tool
    else
      sys.env
        .getOrElse("PATH", "")
        .split(File.pathSeparatorChar)
        .iterator
        .filter(_.nonEmpty)
        .map(dir => new File(dir, tool.getName))
        .find(f => f.isFile && f.canExecute)
        .getOrElse(tool)

  /** Zinc's test for whether a class path entry defines a class, extended to
   *  entries that `snc` wrote, which hold `.nir` and `.tasty` files and no
   *  `.class` files. Without it, Zinc would take classes of an upstream project
   *  compiled by `snc` for missing.
   */
  def definesClass(
      default: VirtualFile => DefinesClass,
      toPath: VirtualFile => Path
  ): VirtualFile => DefinesClass = { entry =>
    val byDefault = default(entry)
    val path = toPath(entry)
    lazy val jarEntries: Set[String] =
      if (!Files.isRegularFile(path)) Set.empty
      else {
        val zip = new java.util.zip.ZipFile(path.toFile)
        try {
          val names = Set.newBuilder[String]
          val it = zip.entries()
          while (it.hasMoreElements()) names += it.nextElement().getName
          names.result()
        } finally zip.close()
      }
    def exists(relative: String): Boolean =
      if (Files.isDirectory(path)) Files.isRegularFile(path.resolve(relative))
      else jarEntries.contains(relative)
    new DefinesClass {
      override def apply(className: String): Boolean =
        byDefault(className) || {
          val file = className.replace('.', '/')
          exists(file + ".nir") ||
            exists(
              SncAnalysis.topLevelName(className).replace('.', '/') + ".tasty"
            )
        }
    }
  }

  /** Identifies an `snc` executable for Zinc's setup, so that a different
   *  binary recompiles everything.
   */
  def identity(snc: File): String = {
    val file = snc.getAbsoluteFile
    s"${file.getPath}:${file.length}:${file.lastModified}"
  }

  /** A digest of the class path's contents, for Zinc's setup when `snc` does
   *  not stream the analysis. Without dependencies Zinc cannot pick what to
   *  recompile, so a change of the digest recompiles everything.
   */
  def classpathDigest(classpath: Seq[Path]): String = {
    val digest = MessageDigest.getInstance("SHA-256")
    def add(s: String): Unit = digest.update(s.getBytes("UTF-8"))
    classpath.foreach { entry =>
      add(entry.toString)
      val file = entry.toFile
      if (file.isDirectory)
        regularFiles(entry).map(_.toFile).foreach { f =>
          add(s"${f.getPath}:${f.length}:${f.lastModified}")
        }
      else if (file.isFile) add(s":${file.length}:${file.lastModified}")
    }
    digest.digest().map("%02x".format(_)).mkString
  }

  private def outputDirectory(output: Output): Path = {
    val single = output.getSingleOutputAsPath()
    if (!single.isPresent)
      throw new IllegalArgumentException(
        "snc writes to a single output directory"
      )
    val path = single.get
    if (path.getFileName.toString.endsWith(".jar"))
      throw new IllegalArgumentException("snc cannot write to a jar")
    Files.createDirectories(path)
    path
  }

  /** Moves the files under `from` to the same place under `to`. */
  private def moveTree(from: Path, to: Path): Seq[Path] =
    regularFiles(from).map { file =>
      val target = to.resolve(from.relativize(file).toString)
      Files.createDirectories(target.getParent)
      Files.move(file, target, StandardCopyOption.REPLACE_EXISTING)
      target
    }

  /** Deletes `path` and everything under it, if it exists. */
  private def delete(path: Path): Unit =
    if (Files.exists(path)) {
      val all = ListBuffer.empty[Path]
      val files = Files.walk(path)
      try {
        val it = files.iterator()
        while (it.hasNext) all += it.next()
      } finally files.close()
      all.reverseIterator.foreach(Files.delete)
    }

  /** The regular files under `directory`, sorted. */
  private def regularFiles(directory: Path): Seq[Path] = {
    val found = ListBuffer.empty[Path]
    val files = Files.walk(directory)
    try {
      val it = files.iterator()
      while (it.hasNext) {
        val file = it.next()
        if (Files.isRegularFile(file)) found += file
      }
    } finally files.close()
    found.toList.sortBy(_.toString)
  }
}

private[scalanative] final class SncCompileFailed(
    args: Array[String],
    reported: Array[Problem]
) extends xsbti.CompileFailed {
  override def arguments(): Array[String] = args
  override def problems(): Array[Problem] = reported
  override def getMessage(): String = "Compilation failed"
  override def toString(): String = "Compilation failed"
}
