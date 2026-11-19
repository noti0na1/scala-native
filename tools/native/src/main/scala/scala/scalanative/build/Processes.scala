package scala.scalanative.build

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

import scala.sys.process._

import scala.scalanative.meta.LinktimeInfo

/** How the toolchain runs clang and other programs. A multithreaded program
 *  uses `scala.sys.process`, as on the JVM. A single-threaded one, such as
 *  `snc` with the extension linker built in, cannot start the threads that copy
 *  a process's output: it sends the output to files and reads them when the
 *  process has ended.
 */
private[scalanative] object Processes {

  /** Runs `command` in `workDir` and returns its exit code. Its standard output
   *  and standard error go to `logger` line by line, in a single-threaded
   *  program once it has ended.
   */
  def run(command: Seq[String], workDir: File, logger: ProcessLogger): Int =
    if (LinktimeInfo.isMultithreadingEnabled)
      Process(command, workDir) ! logger
    else {
      val (exit, out, err) = runToFiles(command, Some(workDir))
      out.foreach(line => logger.out(line))
      err.foreach(line => logger.err(line))
      exit
    }

  /** Returns the lines `command` writes on standard output, whatever its exit
   *  code, and drops its standard error. Throws if it cannot be started.
   */
  def lines(command: Seq[String]): List[String] =
    if (LinktimeInfo.isMultithreadingEnabled)
      Process(command).lineStream_!(ProcessLogger(_ => (), _ => ())).toList
    else runToFiles(command, None)._2

  private def runToFiles(
      command: Seq[String],
      workDir: Option[File]
  ): (Int, List[String], List[String]) = {
    val out = Files.createTempFile("scalanative-process", ".out")
    val err = Files.createTempFile("scalanative-process", ".err")
    try {
      val builder = new java.lang.ProcessBuilder(
        java.util.Arrays.asList(command: _*)
      ).redirectOutput(out.toFile())
      builder.redirectError(err.toFile())
      workDir.foreach(builder.directory(_))
      val process = builder.start()
      process.getOutputStream().close()
      val exit = process.waitFor()
      def read(path: java.nio.file.Path): List[String] = {
        val text = new String(Files.readAllBytes(path), UTF_8)
        if (text.isEmpty) Nil
        else text.split('\n').toList.map(_.stripSuffix("\r"))
      }
      (exit, read(out), read(err))
    } finally {
      Files.deleteIfExists(out)
      Files.deleteIfExists(err)
    }
  }
}
