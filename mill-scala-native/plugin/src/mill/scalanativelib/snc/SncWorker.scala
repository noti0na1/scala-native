package mill.scalanativelib.snc

import java.net.{URL, URLClassLoader}
import java.util.concurrent.ConcurrentHashMap

import mill.api.PathRef
import mill.api.daemon.internal

/** Calls the worker, `scala.scalanative.millplugin.SncMillWorker`, in a class
 *  loader of its own: the worker has its own Scala library and Zinc, at the
 *  version Mill uses, which Mill keeps in its own worker's class loader.
 */
final class SncWorker private (loader: ClassLoader) {
  private val cls =
    loader.loadClass("scala.scalanative.millplugin.SncMillWorker")

  private def call[A](name: String, args: AnyRef*): A = {
    val method = cls.getMethods
      .find(_.getName == name)
      .getOrElse(throw new NoSuchMethodException(s"SncMillWorker.$name"))
    val thread = Thread.currentThread()
    val saved = thread.getContextClassLoader
    thread.setContextClassLoader(loader)
    try method.invoke(null, args*).asInstanceOf[A]
    catch {
      case e: java.lang.reflect.InvocationTargetException => throw e.getCause
    } finally thread.setContextClassLoader(saved)
  }

  /** Compiles `sources` with Zinc around `snc`, keeping the analysis in
   *  `analysisFile`; `sncOptions` go to `snc` after `options`. `upstream` pairs
   *  dependencies' class directories with their analysis files. Zinc's messages
   *  go to `log` as (level, text), its reporter's problems to `problem`, and
   *  its summaries to `summary`. Returns why the compilation failed, if it did.
   */
  def compile(
      snc: os.Path,
      sncOptions: Seq[String],
      workingDirectory: os.Path,
      scalaVersion: String,
      sources: Seq[os.Path],
      classpath: Seq[os.Path],
      options: Seq[String],
      classes: os.Path,
      analysisFile: os.Path,
      upstream: Seq[(os.Path, os.Path)],
      incremental: Boolean,
      reportCachedProblems: Boolean,
      debugEnabled: Boolean,
      log: (String, String) => Unit,
      problem: SncWorker.Problem => Unit,
      summary: () => Unit
  ): Option[String] = {
    val logTo: java.util.function.BiConsumer[String, String] =
      (level, message) => log(level, message)
    val problemTo: java.util.function.Consumer[Array[String]] =
      fields => problem(new SncWorker.Problem(fields))
    val summaryTo: Runnable = () => summary()
    Option(
      call[String](
        "compile",
        snc.toString,
        sncOptions.toArray,
        workingDirectory.toString,
        scalaVersion,
        sources.map(_.toString).toArray,
        classpath.map(_.toString).toArray,
        options.toArray,
        classes.toString,
        analysisFile.toString,
        upstream.map(_._1.toString).toArray,
        upstream.map(_._2.toString).toArray,
        Boolean.box(incremental),
        Boolean.box(reportCachedProblems),
        Boolean.box(debugEnabled),
        logTo,
        problemTo,
        summaryTo
      )
    )
  }

  /** The main classes that a module's analysis records. */
  def mainClasses(analysisFile: os.Path): Seq[String] =
    call[Array[String]]("mainClasses", analysisFile.toString).toSeq
}

object SncWorker {

  /** A problem that `snc` reported, for Mill's reporter (BSP), from the fields
   *  of `SncMillWorker.problemFields`.
   */
  final class Problem(fields: Array[String]) extends internal.Problem {
    private def field(i: Int): Option[String] =
      fields.lift(i).filter(_.nonEmpty)
    private def int(i: Int): Option[Int] = field(i).flatMap(_.toIntOption)

    override def severity: internal.Severity = fields(0) match {
      case "error" => internal.Error
      case "warn"  => internal.Warn
      case _       => internal.Info
    }
    override def category: String = field(1).getOrElse("")
    override def message: String = field(2).getOrElse("")

    /** The text as `snc` renders it. */
    def rendered: String = field(3).getOrElse(message)

    override val position: internal.ProblemPosition =
      new internal.ProblemPosition {
        def sourcePath: Option[String] = field(4)
        def sourceFile: Option[java.io.File] =
          sourcePath.map(new java.io.File(_))
        def line: Option[Int] = int(5)
        def lineContent: String = field(6).getOrElse("")
        def offset: Option[Int] = int(7)
        def pointer: Option[Int] = int(8)
        def pointerSpace: Option[String] = field(9)
        def startOffset: Option[Int] = int(10)
        def endOffset: Option[Int] = int(11)
        def startLine: Option[Int] = int(12)
        def startColumn: Option[Int] = int(13)
        def endLine: Option[Int] = int(14)
        def endColumn: Option[Int] = int(15)
      }

    override def diagnosticCode: Option[internal.DiagnosticCode] =
      field(16).map { c =>
        new internal.DiagnosticCode {
          def code: String = c
          def explanation: Option[String] = field(17)
        }
      }
  }

  private val cache = new ConcurrentHashMap[Seq[PathRef], SncWorker]()

  /** A worker for `classpath`, shared by all modules of the build. The key
   *  includes the entries' signatures, so a republished worker gets a new class
   *  loader.
   */
  def apply(classpath: Seq[PathRef]): SncWorker =
    cache.computeIfAbsent(
      classpath,
      cp => {
        val urls: Array[URL] = cp.map(_.path.toNIO.toUri.toURL).toArray
        new SncWorker(
          new URLClassLoader(
            "snc-mill-worker",
            urls,
            ClassLoader.getPlatformClassLoader
          )
        )
      }
    )
}
