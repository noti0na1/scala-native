package scala.scalanative.sbtplugin.snc

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.util.Optional
import java.util.regex.Pattern

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.NonFatal

import xsbti.{
  Action, DiagnosticCode, DiagnosticRelatedInformation, Position, Problem,
  Severity, TextEdit, WorkspaceEdit
}

/** Turns what `snc` reports into Zinc problems: `diagnostic` events from
 *  `snc --events`, or, for an `snc` without events, the console reporter's
 *  plain output (`-color:never`):
 *
 *  {{{
 *  -- [E007] Type Mismatch Error: /abs/path/A.scala:5:17 ------------------
 *  5 |    val x: Int = "s"
 *    |                 ^^^
 *    |                 Found:    ("s" : String)
 *    |                 Required: Int
 *  }}}
 *
 *  The problems carry what `scalac`'s Zinc bridge gives Zinc: the message (with
 *  the explanation appended under `-explain`), the text as `scalac` prints it
 *  (`rendered`), the error code and its explanation, related positions and
 *  quick fixes (actions).
 */
private[scalanative] object SncDiagnostics {

  final case class Parsed(problems: List[Problem], otherLines: List[String])

  /** Under `-explain`, the explanation's heading in the rendered text. */
  private val ExplainedHeading = "Explanation (enabled by `-explain`)"

  private val LongerExplanation =
    "longer explanation available when compiling with `-explain`"

  /** The message as `scalac`'s Zinc bridge reports it: under `-explain`, the
   *  message, a line break and the explanation under an "Explanation" heading
   *  (blue when `colored`); otherwise the message alone.
   */
  def message(
      message: String,
      explanation: Option[String],
      explained: Boolean,
      colored: Boolean
  ): String =
    explanation.filter(_ => explained) match {
      case Some(text) =>
        val eol = System.lineSeparator
        def blue(s: String) =
          if (colored) Console.BLUE + s + Console.RESET else s
        val withHeading = message + eol + "\n" + blue("Explanation") + "\n" +
          blue("===========") + eol + text
        if (text.endsWith(eol)) withHeading else withHeading + eol
      case None => message
    }

  private val Ansi = Pattern.compile("\u001b\\[[0-9;]*m")
  private val LeadingResets = Pattern.compile("^(\u001b\\[0?m)+")

  private def plain(text: String): String = Ansi.matcher(text).replaceAll("")

  /** `snc` sends the message without colours, and `scalac`'s bridge reports it
   *  as the compiler rendered it, coloured when the rendering is. This is the
   *  message as it appears in `rendered`, under the source line and its
   *  pointer: the lines after a margin (` |`) and a padding of spaces, which
   *  stripped of colours are `message`. `None` when `rendered` has no colours
   *  or does not hold the message so.
   */
  def coloredMessage(rendered: String, message: String): Option[String] =
    if (!rendered.contains("\u001b[")) None
    else {
      // The text after each line's margin, without the margin's reset.
      val contents = rendered.split("\r?\n", -1).toVector.map { line =>
        val bar = plain(line).indexOf('|')
        if (bar < 0 || !Margin.matcher(plain(line)).matches()) None
        else {
          // The bar is the first `|` of the coloured line too: the margin
          // holds only the line number before it.
          val after = line.substring(line.indexOf('|') + 1)
          Some(LeadingResets.matcher(after).replaceFirst(""))
        }
      }
      val lines = message.split("\r?\n", -1).toVector
      def matchesAt(start: Int, padding: Int): Boolean =
        start + lines.size <= contents.size &&
          lines.indices.forall { i =>
            contents(start + i).exists { c =>
              val expected =
                if (lines(i).isEmpty) "" else " " * padding + lines(i)
              plain(c) == expected
            }
          }
      // The message follows the pointer under the source line.
      val pointer = contents.indexWhere(
        _.exists(c => Pointer.matcher(plain(c)).matches())
      )
      val found = (pointer + 1 until contents.size).iterator.flatMap { start =>
        contents(start).flatMap { c =>
          val text = plain(c)
          val padding = text.length - lines.head.length
          if (lines.head.nonEmpty && padding >= 0 && text.endsWith(
                lines.head
              ) &&
              text.take(padding).forall(_ == ' ') && matchesAt(start, padding))
            Some((start, padding))
          else None
        }
      }
      if (!found.hasNext) None
      else {
        val (start, padding) = found.next()
        val colored = lines.indices.map { i =>
          val c = contents(start + i).get
          if (lines(i).isEmpty) "" else c.drop(padding)
        }
        Some(colored.mkString("\n")).filter(plain(_) == message)
      }
    }

  /** Problems from `diagnostic` events. `sourceId` maps a source's path, as
   *  `snc` was given it, to its id for Zinc, so that Zinc files the problem
   *  under its source.
   */
  def fromEvents(
      events: Seq[Any],
      sourceId: String => String
  ): List[Problem] = {
    import SncJson.Fields
    val sources = new Sources
    events.toList.filter(_.string("event").contains("diagnostic")).map { e =>
      val severity = e.string("severity") match {
        case Some("error")   => Severity.Error
        case Some("warning") => Severity.Warn
        case _               => Severity.Info
      }
      val text = e.string("message").getOrElse("")
      val rendered = e.string("rendered").getOrElse(text)
      val explanation = e.string("explanation").filter(_.nonEmpty)
      val related = e.array("related").toList.map { r =>
        relatedInformation(
          eventPosition(r.field("position"), sources, sourceId),
          r.string("message").getOrElse("")
        )
      }
      val actions = e.array("actions").toList.map { a =>
        val edits = a.array("patches").toList.map { p =>
          val file = p.string("file")
          textEdit(
            file.fold(SncPosition.empty)(f =>
              sources.span(f, sourceId(f), p.int("start"), p.int("end"))
            ),
            p.string("replacement").getOrElse("")
          )
        }
        action(a.string("title").getOrElse(""), a.string("description"), edits)
      }
      new SncProblem(
        kind = e.string("kind").getOrElse(""),
        level = severity,
        text = message(
          coloredMessage(rendered, text).getOrElse(text),
          explanation,
          explained = plain(rendered).contains(ExplainedHeading),
          colored = rendered.contains("\u001b[")
        ),
        pos = eventPosition(e.field("position"), sources, sourceId),
        renderedText = rendered,
        code = e.string("code").map(_.stripPrefix("E").dropWhile(_ == '0')),
        explanation = explanation,
        related = related,
        fixes = actions
      )
    }
  }

  /** A position of an event: null, or `{file, line, column, start, end}` with
   *  `line` and `column` counting from 1 and `start` and `end` as `{offset,
   *  line, column}`.
   */
  private def eventPosition(
      pos: Any,
      sources: Sources,
      sourceId: String => String
  ): Position = {
    import SncJson.Fields
    val path = pos.string("file")
    if (path.isEmpty) SncPosition.empty
    else {
      val start = pos.field("start")
      val end = pos.field("end")
      val line = pos.int("line")
      new SncPosition(
        path = path,
        id = path.map(sourceId),
        lineNumber = line,
        column = pos.int("column").map(_ - 1),
        content =
          for (p <- path; l <- line; text <- sources.line(p, l))
            yield text,
        offsets = (start.int("offset"), end.int("offset")),
        startAt = (start.int("line"), start.int("column").map(_ - 1)),
        endAt = (end.int("line"), end.int("column").map(_ - 1))
      )
    }
  }

  // -- [E007] Type Mismatch Error: /path/A.scala:5:17 -----
  // -- Warning: /path/A.scala:5:17
  // -- Error: ------
  private val Header = Pattern.compile(
    """^-- (?:\[E(\d+)\] )?(.*?(Error|Warning|Info))(?:: (.*?):(\d+):(\d+))?:?\s*-*\s*$"""
  )
  private val Margin = Pattern.compile("""^\s*(\d*)\s*\|(.*)$""")
  private val Pointer = Pattern.compile("""^\s*\^+\s*$""")
  private val Summary = Pattern.compile("""^\d+ (errors?|warnings?) found$""")
  // The rule that opens the explanation's box, after the bar: `-----...`
  private val BoxRule = Pattern.compile("""^-{3,}\s*$""")
  // The soft rule under the explanation's heading: `- - - ...`
  private val SoftRule = Pattern.compile("""^-( -)+\s*$""")
  // The line that closes the box, without a bar: `   -----...`
  private val BoxEnd = Pattern.compile("""^\s+-{3,}\s*$""")

  /** Problems from console output. A header starts each diagnostic; the lines
   *  after it that start with a margin (`5 |` or `  |`) belong to it, and so
   *  does the line that closes an explanation's box. Other lines are summaries
   *  (`2 errors found`), which are dropped, or messages without a position.
   */
  def parse(lines: Seq[String], sourceId: String => String): Parsed = {
    val problems = ListBuffer.empty[Problem]
    val others = ListBuffer.empty[String]
    var rest = lines.toList
    while (rest.nonEmpty) {
      val header = Header.matcher(rest.head)
      if (header.matches()) {
        val (margins, after) = rest.tail.span(Margin.matcher(_).matches())
        val explained = margins.exists(_.contains(ExplainedHeading))
        val (body, remaining) = after match {
          case end :: more if explained && BoxEnd.matcher(end).matches() =>
            (margins :+ end, more)
          case _ => (margins, after)
        }
        problems += problem(header, rest.head, margins, body, sourceId)
        rest = remaining
      } else {
        if (!Summary.matcher(rest.head.trim).matches()) others += rest.head
        rest = rest.tail
      }
    }
    Parsed(problems.toList, others.toList)
  }

  private def problem(
      header: java.util.regex.Matcher,
      headerLine: String,
      marginLines: List[String],
      body: List[String],
      sourceId: String => String
  ): Problem = {
    val code = Option(header.group(1))
    val kind = header.group(2)
    val severity = header.group(3) match {
      case "Error"   => Severity.Error
      case "Warning" => Severity.Warn
      case _         => Severity.Info
    }
    val path = Option(header.group(4))
    val line = Option(header.group(5)).map(_.toInt)
    val column = Option(header.group(6)).map(_.toInt - 1)

    // Margin lines: (line number or "", content after the bar).
    val margins = marginLines.map { l =>
      val m = Margin.matcher(l)
      m.matches()
      (m.group(1), m.group(2))
    }
    val lineContent = margins.collectFirst {
      case (n, content) if line.exists(_.toString == n) => content
    }
    val lastCodeOrPointer = margins.lastIndexWhere {
      case (n, content) => n.nonEmpty || Pointer.matcher(content).matches()
    }
    val afterCode = margins.drop(lastCodeOrPointer + 1).map(_._2)
    // The message ends where the explanation's box or its hint starts.
    val messageLines = afterCode.takeWhile { content =>
      !BoxRule.matcher(content).matches() &&
      content.trim != LongerExplanation
    }
    val explanation = {
      val inBox = afterCode
        .dropWhile(!_.contains(ExplainedHeading))
        .drop(1)
        .dropWhile(SoftRule.matcher(_).matches())
      if (inBox.isEmpty) None
      else Some(inBox.map(_.stripPrefix(" ")).mkString("\n").trim)
    }
    val text = {
      val nonEmpty = messageLines.filter(_.trim.nonEmpty)
      val indent =
        if (nonEmpty.isEmpty) 0
        else nonEmpty.map(_.takeWhile(_ == ' ').length).min
      val text = messageLines.map(_.drop(indent)).mkString("\n").trim
      if (text.nonEmpty) text else kind
    }
    new SncProblem(
      kind = kind,
      level = severity,
      text = message(
        text,
        explanation,
        explained = explanation.nonEmpty,
        colored = false
      ),
      pos = new SncPosition(
        path = path,
        id = path.map(sourceId),
        lineNumber = line,
        column = column,
        content = lineContent,
        offsets = (None, None),
        startAt = (line, column),
        endAt = (None, None)
      ),
      renderedText = (headerLine :: body).mkString("\n"),
      code = code.map(_.dropWhile(_ == '0')),
      explanation = explanation,
      related = Nil,
      fixes = Nil
    )
  }

  /** Source files read for line contents and for the positions of quick fixes,
   *  which `snc` gives as offsets only.
   */
  private final class Sources {
    private val texts = mutable.Map.empty[String, Option[Text]]

    private def text(path: String): Option[Text] =
      texts.getOrElseUpdate(path, Text.read(path))

    /** Line `line` (counting from 1) of `path`. */
    def line(path: String, line: Int): Option[String] =
      text(path).flatMap(_.line(line))

    /** The span of `path` from offset `start` to `end`. */
    def span(
        path: String,
        id: String,
        start: Option[Int],
        end: Option[Int]
    ): Position = {
      val file = text(path)
      def at(offset: Option[Int]): (Option[Int], Option[Int]) =
        (for (f <- file; o <- offset) yield f.lineAndColumn(o)) match {
          case Some((l, c)) => (Some(l), Some(c))
          case None         => (None, None)
        }
      val startAt = at(start)
      new SncPosition(
        path = Some(path),
        id = Some(id),
        lineNumber = startAt._1,
        column = startAt._2,
        content = for (f <- file; l <- startAt._1; t <- f.line(l)) yield t,
        offsets = (start, end),
        startAt = startAt,
        endAt = at(end)
      )
    }
  }

  private final class Text(content: String) {
    private val lines: IndexedSeq[String] =
      content.split("\r?\n", -1).toIndexedSeq
    private val lineStarts: Array[Int] = {
      val starts = mutable.ArrayBuffer(0)
      var i = 0
      while (i < content.length) {
        if (content.charAt(i) == '\n') starts += i + 1
        i += 1
      }
      starts.toArray
    }

    def line(number: Int): Option[String] =
      if (number >= 1) lines.lift(number - 1) else None

    /** The line (counting from 1) and column (from 0) of a character offset. */
    def lineAndColumn(offset: Int): (Int, Int) = {
      val found = java.util.Arrays.binarySearch(lineStarts, offset)
      val index = if (found >= 0) found else -found - 2
      (index + 1, offset - lineStarts(index))
    }
  }

  private object Text {
    def read(path: String): Option[Text] =
      try {
        val bytes = Files.readAllBytes(new File(path).toPath)
        Some(new Text(new String(bytes, StandardCharsets.UTF_8)))
      } catch { case NonFatal(_) => None }
  }

  /** Lines are 1-based; columns, `pointer` and offsets are 0-based. */
  private final class SncPosition(
      path: Option[String],
      id: Option[String],
      lineNumber: Option[Int],
      column: Option[Int],
      content: Option[String],
      offsets: (Option[Int], Option[Int]),
      startAt: (Option[Int], Option[Int]),
      endAt: (Option[Int], Option[Int])
  ) extends Position {
    override def line(): Optional[Integer] = optionalInt(lineNumber)
    override def lineContent(): String = content.getOrElse("")
    override def offset(): Optional[Integer] = optionalInt(offsets._1)
    override def pointer(): Optional[Integer] = optionalInt(column)
    override def pointerSpace(): Optional[String] =
      (content, column) match {
        case (Some(text), Some(c)) =>
          Optional.of(text.take(c).map(ch => if (ch == '\t') '\t' else ' '))
        case _ => Optional.empty()
      }
    override def sourcePath(): Optional[String] = optional(id)
    override def sourceFile(): Optional[File] = optional(path.map(new File(_)))
    override def startOffset(): Optional[Integer] = optionalInt(offsets._1)
    override def endOffset(): Optional[Integer] = optionalInt(offsets._2)
    override def startLine(): Optional[Integer] = optionalInt(startAt._1)
    override def startColumn(): Optional[Integer] = optionalInt(startAt._2)
    override def endLine(): Optional[Integer] = optionalInt(endAt._1)
    override def endColumn(): Optional[Integer] = optionalInt(endAt._2)
    override def toString(): String =
      path.fold("")(p => s"$p:${lineNumber.getOrElse(0)}")
  }

  private object SncPosition {
    val empty: Position = new SncPosition(
      None,
      None,
      None,
      None,
      None,
      (None, None),
      (None, None),
      (None, None)
    )
  }

  private def relatedInformation(
      pos: Position,
      text: String
  ): DiagnosticRelatedInformation =
    new DiagnosticRelatedInformation {
      override def position(): Position = pos
      override def message(): String = text
    }

  private def textEdit(pos: Position, text: String): TextEdit =
    new TextEdit {
      override def position(): Position = pos
      override def newText(): String = text
    }

  private def action(
      name: String,
      about: Option[String],
      edits: List[TextEdit]
  ): Action = {
    val workspaceEdit = new WorkspaceEdit {
      override def changes(): java.util.List[TextEdit] = javaList(edits)
    }
    new Action {
      override def title(): String = name
      override def description(): Optional[String] = optional(about)
      override def edit(): WorkspaceEdit = workspaceEdit
    }
  }

  private final class SncProblem(
      kind: String,
      level: Severity,
      text: String,
      pos: Position,
      renderedText: String,
      code: Option[String],
      explanation: Option[String],
      related: List[DiagnosticRelatedInformation],
      fixes: List[Action]
  ) extends Problem {
    override def category(): String = kind
    override def severity(): Severity = level
    override def message(): String = text
    override def position(): Position = pos
    override def rendered(): Optional[String] = Optional.of(renderedText)
    override def diagnosticCode(): Optional[DiagnosticCode] =
      optional(code.filter(_.nonEmpty).map { c =>
        new DiagnosticCode {
          override def code(): String = c
          override def explanation(): Optional[String] =
            optional(SncProblem.this.explanation)
        }
      })
    override def diagnosticRelatedInformation()
        : java.util.List[DiagnosticRelatedInformation] = javaList(related)
    override def actions(): java.util.List[Action] = javaList(fixes)
    override def toString(): String = renderedText
  }

  private def javaList[T](values: List[T]): java.util.List[T] = {
    val list = new java.util.ArrayList[T](values.size)
    values.foreach(list.add)
    java.util.Collections.unmodifiableList(list)
  }

  private def optional[T](value: Option[T]): Optional[T] =
    value.fold(Optional.empty[T]())(Optional.of(_))

  private def optionalInt(value: Option[Int]): Optional[Integer] =
    value.fold(Optional.empty[Integer]())(v => Optional.of(Integer.valueOf(v)))
}
