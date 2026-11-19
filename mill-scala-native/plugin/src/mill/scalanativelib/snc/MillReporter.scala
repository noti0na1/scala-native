package mill.scalanativelib.snc

import scala.collection.mutable

import mill.api.daemon.internal

/** Prints `snc`'s problems and Zinc's messages as Mill prints those of its own
 *  Zinc compiles with `scalac`, and passes the problems on to Mill's reporter
 *  (BSP).
 *
 *  Mill's Zinc worker (`mill-libs-javalib-worker`) logs through Zinc's
 *  `LoggedReporter` with three changes: each problem's rendered text becomes
 *  Mill's own format (its `TransformingReporter`: the path relative to the
 *  workspace, one source line, a pointer under the span, the message), the
 *  level shows once per message (`[error] `, `[warn] `, none for info) on the
 *  task's info stream, and paths in "compiling ..." are relative. This class
 *  does the same with the problems the worker sends, using Mill's formatting
 *  functions, so that `snc` modules and `scalac` modules print alike:
 *
 *  {{{
 *  [warn] lib/src/lib/Greeting.scala:12:7
 *    def unused = 1
 *        ^^^^^^
 *  ...the message...
 *
 *  [warn] one warning found
 *  }}}
 *
 *  As in Zinc's reporter, a warning or error at an offset that already had one
 *  of the same or a higher severity is not shown again, errors beyond
 *  `maxErrors` are counted but not shown, and only shown problems reach
 *  `forward`.
 */
private[snc] final class MillReporter(
    log: mill.api.Logger,
    colored: Boolean,
    workspaceRoot: os.Path,
    maxErrors: Int,
    forward: Option[internal.CompileProblemReporter]
) {
  private val positions = mutable.Map.empty[(Int, String), internal.Severity]
  private val counts = mutable.Map.empty[internal.Severity, Int]

  private def count(severity: internal.Severity): Int =
    counts.getOrElse(severity, 0)

  /** A message of Zinc's logger, at `level` (`error`, `warn`, `info` or
   *  `debug`).
   */
  def message(level: String, text: String): Unit = level match {
    case "error" => log.info(prefix(internal.Error) + text)
    case "warn"  => log.info(prefix(internal.Warn) + text)
    case "info" => log.info(MillReporter.relativeCompiling(text, workspaceRoot))
    case _      => log.debug(text)
  }

  def problem(p: SncWorker.Problem): Unit = {
    val repeated = p.severity != internal.Info && {
      val pos = p.position
      (pos.offset, pos.sourceFile) match {
        case (Some(offset), Some(file)) =>
          val key = (offset, file.getPath)
          val seen =
            positions.get(key).exists(rank(_) >= rank(p.severity))
          if (!seen) positions(key) = p.severity
          seen
        case _ => false
      }
    }
    if (!repeated) {
      counts(p.severity) = count(p.severity) + 1
      if (p.severity != internal.Error || maxErrors <= 0 ||
          count(internal.Error) <= maxErrors) {
        log.info(
          prefix(p.severity) +
            MillReporter.render(colored, p, workspaceRoot)
        )
        forward.foreach { r =>
          p.severity match {
            case internal.Error => r.logError(p)
            case internal.Warn  => r.logWarning(p)
            case internal.Info  => r.logInfo(p)
          }
        }
      }
    }
  }

  /** Zinc's summary: `one warning found`, `two errors found`... counting every
   *  problem since the start of the compilation.
   */
  def printSummary(): Unit = {
    forward.foreach(_.printSummary())
    val warnings = count(internal.Warn)
    if (warnings > 0)
      log.info(
        prefix(internal.Warn) + MillReporter.counted(warnings, "warning") +
          " found"
      )
    val errors = count(internal.Error)
    if (errors > 0)
      log.info(
        prefix(internal.Error) + MillReporter.counted(errors, "error") +
          " found"
      )
  }

  private def rank(severity: internal.Severity): Int = severity match {
    case internal.Info  => 0
    case internal.Warn  => 1
    case internal.Error => 2
  }

  private def prefix(severity: internal.Severity): String = severity match {
    case internal.Error =>
      if (colored) "[" + fansi.Color.Red("error") + "] " else "[error] "
    case internal.Warn =>
      if (colored) "[" + fansi.Color.Yellow("warn") + "] " else "[warn] "
    case internal.Info => ""
  }
}

private[snc] object MillReporter {

  /** Zinc's `countElementsAsString`. */
  def counted(n: Int, element: String): String = n match {
    case 0 => "no " + element + "s"
    case 1 => "one " + element
    case 2 => "two " + element + "s"
    case 3 => "three " + element + "s"
    case 4 => "four " + element + "s"
    case _ => s"$n ${element}s"
  }

  private val Compiling =
    """compiling (\d+) Scala (sources?) to (.*) \.\.\.""".r

  /** "compiling N Scala sources to <path> ...", with the path relative to the
   *  workspace, as Mill shows it.
   */
  def relativeCompiling(text: String, workspaceRoot: os.Path): String =
    text match {
      case Compiling(n, sources, path) =>
        s"compiling $n Scala $sources to ${relative(path, workspaceRoot)} ..."
      case _ => text
    }

  private def relative(path: String, workspaceRoot: os.Path): String = {
    val abs = os.Path(path, workspaceRoot)
    if (abs.startsWith(workspaceRoot)) abs.subRelativeTo(workspaceRoot).toString
    else path
  }

  /** A problem as Mill renders `scalac`'s (its `TransformingReporter`, for
   *  Scala sources): `path:line:column` with the path relative to the
   *  workspace, the source line as the compiler rendered it (coloured when the
   *  compiler coloured it), a pointer under the span, and the message. A
   *  problem without a position is its message.
   */
  def render(
      colored: Boolean,
      p: SncWorker.Problem,
      workspaceRoot: os.Path
  ): String = {
    val pos = p.position
    pos.sourcePath match {
      case None       => p.message
      case Some(path) =>
        val shade: java.util.function.Function[String, String] =
          if (!colored) s => s
          else
            p.severity match {
              case internal.Error => s => Console.RED + s + Console.RESET
              case internal.Warn  => s => Console.YELLOW + s + Console.RESET
              case internal.Info  => s => Console.BLUE + s + Console.RESET
            }
        val absPath = os.Path(path, workspaceRoot)
        val displayPath =
          if (absPath.startsWith(workspaceRoot))
            absPath.subRelativeTo(workspaceRoot).toString
          else path
        val line = pos.line.getOrElse(-1)
        val pointer0 = pos.pointer.getOrElse(-1)
        val space = pos.pointerSpace.getOrElse("")
        val endCol = pos.endColumn.getOrElse(pointer0 + 1)
        val scraped = mill.api.internal.Util.scrapeColoredLineContent(
          p.rendered.linesIterator.toSeq,
          line,
          pos.lineContent
        )
        val lineContent =
          if (scraped.nonEmpty) scraped
          else
            try os.read.lines(absPath).apply(line - 1)
            catch { case _: Exception => "" }
        val plainLineContent = fansi.Str(lineContent).plainText
        val plainLineLength = fansi.Str(lineContent).length
        val colNum = pointer0 + 1
        val displayPointer0 = colNum - 1
        val pointerPrefix =
          if (displayPointer0 > 0 && plainLineContent.nonEmpty)
            plainLineContent
              .take(math.min(displayPointer0, plainLineContent.length))
              .map(c => if (c == '\t') '\t' else ' ')
          else ""
        val pointerLength =
          if (space.nonEmpty && displayPointer0 >= 0 && endCol >= 0)
            math.max(
              1,
              math.min(
                endCol - displayPointer0,
                plainLineLength - space.length
              )
            )
          else if (space.nonEmpty) math.max(1, plainLineLength - space.length)
          else 1
        mill.constants.Util.formatError(
          displayPath,
          line,
          colNum,
          lineContent,
          p.message,
          pointerLength,
          pointerPrefix,
          shade
        )
    }
  }
}
