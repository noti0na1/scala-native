package scala.scalanative.sbtplugin.snc

import java.io.{
  BufferedReader, DataInputStream, EOFException, File, InputStream,
  InputStreamReader
}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap

import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

/** How the sbt plugin runs `snc`, the native Scala 3 compiler.
 *
 *  Every `snc` command line is built here: `scalac`'s options (`-d`,
 *  `-classpath`, `-P:` plugin options, `@argfile`), `-scalanative`, the JDK
 *  whose Java API to compile against (`--java-home`), `--about`, `--events` (a
 *  binary stream of JSON events on standard output) and `--analysis`. An `snc`
 *  that does not answer `--about` gets plain console output, which
 *  [[SncDiagnostics]] parses.
 */
private[scalanative] object SncCommand {

  /** What `snc --about` says, when it says anything. */
  final case class About(json: Any) {
    import SncJson.Fields
    def eventsProtocol: Option[String] = json.string("eventsProtocol")
    def supportsEvents: Boolean = eventsProtocol.exists(_.startsWith("1."))

    /** `snc` streams Zinc's analysis (`--analysis`, protocol 1.1). */
    def supportsAnalysis: Boolean =
      supportsEvents && json.field("analysis") == true
    def compilerVersion: Option[String] = json.string("compilerVersion")
  }

  /** The result of one run. In events mode, `events` holds the parsed events
   *  and `output` standard error; otherwise `output` holds standard output and
   *  standard error, merged.
   */
  final case class Result(
      exitCode: Int,
      events: List[Any],
      output: List[String]
  )

  private val abouts = new ConcurrentHashMap[String, Option[About]]()

  /** `snc --about`, once per executable. */
  def about(snc: Path): Option[About] = {
    val key = SncCompiler.identity(snc.toFile)
    val cached = abouts.get(key)
    if (cached != null) cached
    else {
      val about =
        try {
          val process = new ProcessBuilder(snc.toString, "--about")
            .redirectErrorStream(true)
            .start()
          process.getOutputStream().close()
          val output = readLines(process.getInputStream())
          process.waitFor()
          output.find(_.trim.startsWith("{")).map(l => About(SncJson.parse(l)))
        } catch { case NonFatal(_) => None }
      abouts.put(key, about)
      about
    }
  }

  /** `snc`'s option for the JDK whose Java API it compiles against; without it,
   *  `snc` looks for one itself (`JAVA_HOME`, `java` on the `PATH`, ...).
   */
  def javaHome(home: Path): Seq[String] = Seq("--java-home", home.toString)

  /** The arguments of one compilation, without the executable. An `snc` that
   *  answers `--about` is asked for NIR (`-scalanative`) and events.
   */
  def arguments(
      about: Option[About],
      options: Seq[String],
      classpath: Seq[Path],
      outputDirectory: Path,
      sources: Seq[Path]
  ): Seq[String] = {
    val mode =
      if (about.exists(_.supportsEvents))
        (if (options.contains("-scalanative")) Nil else Seq("-scalanative")) ++
          Seq("--events") ++
          (if (about.exists(_.supportsAnalysis)) Seq("--analysis") else Nil)
      else if (options.exists(_.startsWith("-color"))) Nil
      else Seq("-color:never")
    val classpathOption =
      if (classpath.isEmpty) Nil
      else Seq("-classpath", classpath.mkString(File.pathSeparator))
    options ++ mode ++ Seq("-d", outputDirectory.toString) ++
      classpathOption ++ sources.map(_.toString)
  }

  /** Runs `snc` with `arguments`, passed through `argumentFile` or, when they
   *  do not fit in one, on the command line ([[prepareArguments]]).
   */
  def run(
      snc: Path,
      arguments: Seq[String],
      argumentFile: Path,
      workingDirectory: File,
      onLine: String => Unit
  ): Result = {
    val command = new java.util.ArrayList[String]()
    (snc.toString +: prepareArguments(arguments, argumentFile))
      .foreach(argument => command.add(argument))
    val events = arguments.contains("--events")
    val process =
      new ProcessBuilder(command)
        .directory(workingDirectory)
        .redirectErrorStream(!events)
        .start()
    process.getOutputStream().close()
    if (!events) {
      val output = readLines(process.getInputStream(), onLine)
      Result(process.waitFor(), Nil, output)
    } else {
      val stderr = ListBuffer.empty[String]
      val stderrReader = new Thread("snc-stderr") {
        override def run(): Unit =
          stderr ++= readLines(process.getErrorStream(), onLine)
      }
      stderrReader.setDaemon(true)
      stderrReader.start()
      val parsed =
        try readEvents(process.getInputStream())
        catch {
          case e: Throwable =>
            // Standard output is no event stream: snc is stopped and reaped
            // rather than left running.
            process.destroyForcibly()
            process.waitFor()
            stderrReader.join()
            throw e
        }
      val exitCode = process.waitFor()
      stderrReader.join()
      Result(exitCode, parsed, stderr.toList)
    }
  }

  /** Whether `arguments` can be written to an argument file and read back
   *  unchanged. `snc` reads one as `scalac` does: it drops everything from a
   *  `#` to the end of a line, joins the lines with spaces, takes `"` or `'`
   *  around an argument as quotes, and keeps a backslash, which stops the
   *  character after it from ending a quote or an argument.
   */
  def fitsArgumentFile(arguments: Seq[String]): Boolean =
    arguments.forall { argument =>
      !argument.exists(c => c == '#' || c == '\n' || c == '\r') &&
      !(argument.contains('"') && argument.contains('\'')) &&
      argument.reverseIterator.takeWhile(_ == '\\').size % 2 == 0
    }

  /** The arguments of the `snc` command that runs `arguments`: `@argumentFile`,
   *  or, when they do not fit in an argument file ([[fitsArgumentFile]]),
   *  `arguments` themselves, in the same order.
   */
  def commandArguments(
      arguments: Seq[String],
      argumentFile: Path
  ): Seq[String] =
    if (fitsArgumentFile(arguments)) Seq("@" + argumentFile.toString)
    else arguments

  /** [[commandArguments]], after writing `arguments` to `argumentFile` when
   *  they go through it.
   */
  def prepareArguments(
      arguments: Seq[String],
      argumentFile: Path
  ): Seq[String] = {
    if (fitsArgumentFile(arguments)) writeArgumentFile(argumentFile, arguments)
    else Files.deleteIfExists(argumentFile)
    commandArguments(arguments, argumentFile)
  }

  /** The command that [[run]] executes, for logs and error messages. */
  def show(snc: Path, arguments: Seq[String]): String =
    (snc.toString +: arguments.map(quote)).mkString(" ")

  /** The binary event stream: `SNCE`, the major and minor protocol versions
   *  (16-bit big-endian), then frames of a 32-bit big-endian length, a kind
   *  byte and the payload. Kind 1 is one event as UTF-8 JSON; other kinds are
   *  skipped.
   */
  private def readEvents(stream: InputStream): List[Any] = {
    val in = new DataInputStream(stream)
    val events = ListBuffer.empty[Any]
    try {
      val magic = new Array[Byte](4)
      in.readFully(magic)
      if (new String(magic, StandardCharsets.US_ASCII) != "SNCE")
        throw new IllegalStateException("snc --events: no SNCE header")
      val major = in.readUnsignedShort()
      in.readUnsignedShort()
      if (major != 1)
        throw new IllegalStateException(
          s"snc --events: unsupported protocol version $major"
        )
      var more = true
      while (more) {
        val length =
          try in.readInt()
          catch { case _: EOFException => more = false; 0 }
        if (more) {
          val kind = in.readUnsignedByte()
          val payload = new Array[Byte](length - 1)
          in.readFully(payload)
          if (kind == 1)
            events += SncJson.parse(new String(payload, StandardCharsets.UTF_8))
        }
      }
    } catch {
      // A crash before the header: the exit code and stderr describe it.
      case _: EOFException => ()
    } finally in.close()
    events.toList
  }

  private def readLines(
      stream: InputStream,
      onLine: String => Unit = _ => ()
  ): List[String] = {
    val output = ListBuffer.empty[String]
    val reader =
      new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8))
    try {
      var line = reader.readLine()
      while (line != null) {
        output += line
        onLine(line)
        line = reader.readLine()
      }
    } finally reader.close()
    output.toList
  }

  /** One argument per line, in the syntax `scalac` and `snc` read after `@`:
   *  empty arguments and those with spaces or quotes are quoted.
   */
  private def writeArgumentFile(file: Path, arguments: Seq[String]): Unit = {
    Files.createDirectories(file.getParent())
    val content = arguments.map(quote).mkString("", "\n", "\n")
    Files.write(file, content.getBytes(StandardCharsets.UTF_8))
  }

  private def quote(argument: String): String =
    if (argument.nonEmpty &&
        !argument.exists(c => c.isWhitespace || c == '"' || c == '\''))
      argument
    else if (!argument.contains('"')) "\"" + argument + "\""
    else "'" + argument + "'"
}
