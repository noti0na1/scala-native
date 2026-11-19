package scala.scalanative.build

import java.io.File

import scala.sys.process._

/** How the toolchain runs clang and other programs. On the JVM, through
 *  `scala.sys.process`, which copies their output with threads.
 */
private[scalanative] object Processes {

  /** Runs `command` in `workDir` and returns its exit code. Its standard output
   *  and standard error go to `logger` line by line.
   */
  def run(command: Seq[String], workDir: File, logger: ProcessLogger): Int =
    Process(command, workDir) ! logger

  /** Returns the lines `command` writes on standard output, whatever its exit
   *  code, and drops its standard error. Throws if it cannot be started.
   */
  def lines(command: Seq[String]): List[String] =
    Process(command).lineStream_!(ProcessLogger(_ => (), _ => ())).toList
}
