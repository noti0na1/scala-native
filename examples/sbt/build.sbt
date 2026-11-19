// A library, an application that uses it and os-lib, and their MUnit tests,
// all compiled by snc: the one that $SNC names, else snc on the PATH. Macros
// (os-lib's paths, MUnit's test locations) need snc-host.

// The Scala version that `snc --about` reports
ThisBuild / scalaVersion := "3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY"
ThisBuild / resolvers +=
  "scala-nightlies" at "https://repo.scala-lang.org/artifactory/maven-nightlies"
ThisBuild / nativeScalaCompiler := Some(file(sys.env.getOrElse("SNC", "snc")))

val munit = Def.setting("org.scalameta" %%% "munit" % "1.2.1" % Test)

lazy val lib = project
  .enablePlugins(ScalaNativePlugin)
  .settings(libraryDependencies += munit.value)

lazy val app = project
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(lib)
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %%% "os-lib" % "0.11.8",
      munit.value
    )
  )
