package build

import sbt.Keys._
import sbt._

import java.io.File

/** A development option for where this build's local artifacts go.
 *
 *  By default every project publishes locally into the default Ivy home
 *  (`~/.ivy2/local`, or `<sbt.ivy.home>/local`), and so does the scalalib that
 *  scala3lib publishes for itself before its `update`. With
 *  `-Dscalanative.ivyHome=<dir>`, or the environment variable
 *  `SCALANATIVE_IVY_HOME`, every project uses `<dir>` as its Ivy home instead:
 *  it publishes into `<dir>/local` and resolves local artifacts from there,
 *  leaving `~/.ivy2/local` untouched. Nested sbt runs (scripted tests) get the
 *  same home through `sbt.ivy.home`.
 */
object DevIvyHomePlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = plugins.JvmPlugin

  lazy val devIvyHome: Option[File] =
    sys.props
      .get("scalanative.ivyHome")
      .orElse(sys.env.get("SCALANATIVE_IVY_HOME"))
      .map(_.trim)
      .filter(_.nonEmpty)
      .map(new File(_).getAbsoluteFile)

  override def projectSettings: Seq[Setting[_]] =
    devIvyHome.toSeq.map { home =>
      ivyPaths := ivyPaths.value.withIvyHome(home.getPath)
    }
}
