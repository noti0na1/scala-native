import java.util.concurrent.TimeUnit

enablePlugins(ScalaNativePlugin)

scalaVersion := {
  val scalaVersion = System.getProperty("scala.version")
  if (scalaVersion == null)
    throw new RuntimeException(
      """|The system property 'scala.version' is not defined.
         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin
    )
  else scalaVersion
}

nativeConfig ~= { _.withMultithreading(true) }

// sbt 1 links to a File, sbt 2 to a VirtualFileRef
def nativeExecutable(
    linkOutput: Any
)(implicit conv: xsbti.FileConverter): java.io.File =
  linkOutput match {
    case f: java.io.File           => f
    case ref: xsbti.VirtualFileRef => conv.toPath(ref).toFile()
  }

def runBinary(binary: java.io.File, env: Map[String, String]): (Int, String) = {
  val pb = new ProcessBuilder(binary.getAbsolutePath)
  env.foreach { case (k, v) => pb.environment().put(k, v) }
  pb.redirectErrorStream(true)
  val log = java.io.File.createTempFile("address-sanitizer-", ".log")
  pb.redirectOutput(log)
  val proc = pb.start()
  if (!proc.waitFor(300L, TimeUnit.SECONDS)) {
    proc.destroyForcibly()
    throw new RuntimeException("binary did not finish within 300 seconds")
  }
  val output = new String(java.nio.file.Files.readAllBytes(log.toPath))
  log.delete()
  (proc.exitValue(), output)
}

lazy val checkSanitized = taskKey[Unit]("Run the program under ASan")
checkSanitized := {
  (Compile / nativeConfig).value.sanitizer match {
    case None =>
      println("address-sanitizer: skipped (no sanitizer on this platform)")
    case Some(_) =>
      implicit val conv: xsbti.FileConverter = Keys.fileConverter.value
      val binary = nativeExecutable((Compile / nativeLink).value)

      val (code, output) = runBinary(binary, Map.empty)
      print(output)
      if (code != 0 || !output.contains("done"))
        throw new RuntimeException(s"the program exited with code $code")

      val fakeStack = "detect_stack_use_after_return=1"
      val (refusedCode, refusedOutput) =
        runBinary(binary, Map("ASAN_OPTIONS" -> fakeStack))
      print(refusedOutput)
      if (refusedCode != 1 ||
          !refusedOutput.contains(
            "detect_stack_use_after_return is not supported"
          ))
        throw new RuntimeException(
          s"with $fakeStack the program exited with code $refusedCode"
        )
      println("address-sanitizer: passed")
  }
}
