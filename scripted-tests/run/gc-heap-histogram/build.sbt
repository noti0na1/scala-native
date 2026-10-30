enablePlugins(ScalaNativePlugin)

Compile / mainClass := Some("Main")

scalaVersion := {
  val scalaVersion = System.getProperty("scala.version")
  if (scalaVersion == null)
    throw new RuntimeException(
      """|The system property 'scala.version' is not defined.
         |Specify this property using the scriptedLaunchOpts -D.""".stripMargin
    )
  else scalaVersion
}

def nativeExecutable(
    linkOutput: Any
)(implicit conv: xsbti.FileConverter): java.io.File =
  linkOutput match {
    case f: java.io.File           => f
    case ref: xsbti.VirtualFileRef => conv.toPath(ref).toFile()
  }

@transient lazy val checkHistogram = taskKey[Unit](
  "Check the live-heap histogram that GC_HISTOGRAM_FILE writes"
)

checkHistogram := {
  implicit val conv: xsbti.FileConverter = Keys.fileConverter.value
  val binary = nativeExecutable((Compile / nativeLink).value)
  val histogram = target.value / "histogram.tsv"

  val lines = HistogramCheck.run(binary, histogram, top = None)
  HistogramCheck.checkCollections(lines, top = 40)
  HistogramCheck.checkRetained(lines)

  val limited = HistogramCheck.run(binary, histogram, top = Some(3))
  HistogramCheck.checkCollections(limited, top = 3)
}
