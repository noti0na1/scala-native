package scala.scalanative.millplugin

import java.nio.file.{Path, Paths}
import java.util.function.{BiConsumer, Consumer}

/** The entry points of the worker that the Mill plugin loads.
 *
 *  The plugin loads this object in a class loader of its own, with its own
 *  Scala library and the Zinc version that Mill uses, and calls it through
 *  reflection, so every entry point takes and returns Java types only.
 */
object SncMillWorker {

  /** Compiles with Zinc around `snc` ([[SncZinc]]), with `sncOptions` on
   *  `snc`'s command line after `options`. `upstreamClasses(i)` is the class
   *  directory of a module whose analysis is `upstreamAnalyses(i)`. Messages go
   *  to `log` as (level, text), level one of `error`, `warn`, `info`, `debug`;
   *  problems go to `problems` as in [[problemFields]], and Zinc's requests for
   *  a summary of them to `summary`. Returns `null` on success, else why the
   *  compilation failed.
   */
  def compile(
      snc: String,
      sncOptions: Array[String],
      workingDirectory: String,
      scalaVersion: String,
      sources: Array[String],
      classpath: Array[String],
      options: Array[String],
      classesDirectory: String,
      analysisFile: String,
      upstreamClasses: Array[String],
      upstreamAnalyses: Array[String],
      incremental: Boolean,
      reportCachedProblems: Boolean,
      debugEnabled: Boolean,
      log: BiConsumer[String, String],
      problems: Consumer[Array[String]],
      summary: Runnable
  ): String = {
    val request = SncZinc.Request(
      snc = path(snc),
      sncOptions = sncOptions.toSeq,
      workingDirectory = path(workingDirectory),
      scalaVersion = scalaVersion,
      sources = sources.toSeq.map(path),
      classpath = classpath.toSeq.map(path),
      options = options.toSeq,
      classesDirectory = path(classesDirectory),
      analysisFile = path(analysisFile),
      upstreamAnalyses =
        upstreamClasses.map(path).zip(upstreamAnalyses.map(path)).toMap,
      incremental = incremental,
      reportCachedProblems = reportCachedProblems
    )
    // The sink's methods have the parameters' names.
    val showDebug = debugEnabled
    val logTo = log
    val summaryTo = summary
    val sink = new SncZinc.Sink {
      def debugEnabled: Boolean = showDebug
      def log(level: String, message: String): Unit =
        logTo.accept(level, message)
      def problem(p: xsbti.Problem): Unit = problems.accept(problemFields(p))
      def summary(): Unit = summaryTo.run()
    }
    SncZinc.compile(request, sink).orNull
  }

  /** The main classes that a module's analysis records. */
  def mainClasses(analysisFile: String): Array[String] =
    SncZinc.mainClasses(path(analysisFile)).toArray

  /** A problem as strings, for the plugin's class loader: severity (`error`,
   *  `warn`, `info`), category, message, rendered, then the position's source
   *  path, line, line content, offset, pointer, pointer space, start offset,
   *  end offset, start line, start column, end line, end column, and the
   *  diagnostic code and its explanation. Absent values are empty strings.
   */
  def problemFields(p: xsbti.Problem): Array[String] = {
    def opt[A](o: java.util.Optional[A]): String =
      if (o.isPresent) o.get.toString else ""
    val pos = p.position
    val code = p.diagnosticCode
    Array(
      p.severity match {
        case xsbti.Severity.Error => "error"
        case xsbti.Severity.Warn  => "warn"
        case xsbti.Severity.Info  => "info"
      },
      Option(p.category).getOrElse(""),
      Option(p.message).getOrElse(""),
      opt(p.rendered),
      opt(pos.sourcePath),
      opt(pos.line),
      Option(pos.lineContent).getOrElse(""),
      opt(pos.offset),
      opt(pos.pointer),
      opt(pos.pointerSpace),
      opt(pos.startOffset),
      opt(pos.endOffset),
      opt(pos.startLine),
      opt(pos.startColumn),
      opt(pos.endLine),
      opt(pos.endColumn),
      if (code.isPresent) code.get.code else "",
      if (code.isPresent) opt(code.get.explanation) else ""
    )
  }

  private def path(s: String): Path = Paths.get(s).toAbsolutePath.normalize
}
