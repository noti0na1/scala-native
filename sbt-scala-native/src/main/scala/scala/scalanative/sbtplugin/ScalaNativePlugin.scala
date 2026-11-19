package scala.scalanative
package sbtplugin

import sbt._

import scala.language.implicitConversions

import sjsonnew.JsonFormat

object ScalaNativePlugin extends AutoPlugin {
  override def requires: Plugins = plugins.JvmPlugin

  object autoImport {
    type NativeLinkResult = PluginCompat.FileRef

    val ScalaNativeCrossVersion = sbtplugin.ScalaNativeCrossVersion
    val ScalaNativePlatform: String = sbtplugin.ScalaNativePlatform.current

    private[sbtplugin] val nativeOrgName = "org.scala-native"
    val nativeVersion = nir.Versions.current

    def scalalibVersion(scalaVersion: String, nativeVersion: String): String =
      s"$scalaVersion+$nativeVersion"

    /** Declares `Tag`s which may be used to limit the concurrency of build
     *  tasks.
     *
     *  For example, the following snippet can be used to limit the number of
     *  linking tasks which are able to run at once:
     *
     *  {{{
     *  Global / concurrentRestrictions += Tags.limit(NativeTags.Link, 2)
     *  }}}
     */
    object NativeTags {

      /** This tag is applied to the [[nativeLink]] task. */
      val Link = Tags.Tag("native-link")
    }

    /** Adds `bytes`, `kb`, `mb` and `gb` to integers, for the sizes of
     *  [[build.GarbageCollectorConfig]], e.g. `withMaxHeapSize(1.gb)`
     */
    implicit def intToMemorySizeOps(
        value: Int
    ): build.MemorySize.MemorySizeOps =
      build.MemorySize.syntax.intToMemorySizeOps(value)
    implicit def longToMemorySizeOps(
        value: Long
    ): build.MemorySize.MemorySizeOps =
      build.MemorySize.syntax.longToMemorySizeOps(value)

    val nativeConfig =
      taskKey[build.NativeConfig](
        "User configuration for the native build, NativeConfig"
      )

    val nativeLink =
      taskKey[NativeLinkResult]("Generates native binary without running it.")

    /** The `snc` executable, a Scala 3 compiler built with Scala Native that
     *  writes TASTy and NIR. When defined, `compile` runs it instead of the JVM
     *  Scala compiler, against the Java API of the JDK that sbt runs on, as
     *  `scalac` does. Compilation is incremental when `snc` streams Zinc's
     *  analysis (`snc --about` reports `"analysis": true`); otherwise any
     *  change of a source or of the class path recompiles every source of the
     *  configuration. A bare name such as `file("snc")` is looked up on the
     *  `PATH`. `None`, the default, keeps the JVM compiler.
     */
    val nativeScalaCompiler = settingKey[Option[File]](
      "The snc executable; when defined, compile runs it instead of the JVM Scala compiler."
    )

    /** Options that `compile` passes to `snc`, after `scalacOptions`. */
    val nativeScalaCompilerOptions = taskKey[Seq[String]](
      "Options that compile passes to snc, after scalacOptions."
    )

    val nativeLinkReleaseFast =
      taskKey[NativeLinkResult](
        "Generates native binary in release-fast configuration without running it."
      )

    val nativeLinkReleaseFull =
      taskKey[NativeLinkResult](
        "Generates native binary in release-full configuration without running it."
      )

    implicit def nativeConfigJsonFormat: JsonFormat[build.NativeConfig] =
      NativeConfigJsonFormats.NativeConfigCodec
  }

  override def globalSettings: Seq[Setting[_]] =
    ScalaNativePluginInternal.scalaNativeGlobalSettings

  override def projectSettings: Seq[Setting[_]] =
    ScalaNativePluginInternal.scalaNativeProjectSettings
}
