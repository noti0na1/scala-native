# Scala Native, with a native toolchain and live execution

This is a fork of [Scala Native](https://github.com/scala-native/scala-native) (based on its `main`) with what a Scala 3 compiler built with Scala Native needs: [snc](#snc-the-native-scala-3-compiler), whose sources are in a companion fork of Scala 3. It adds:

- **`snlink`**, the Scala Native toolchain as a native command-line linker: it links Scala Native programs and libraries without a JVM.
- **Compiling with `snc` in sbt**: the sbt plugin's `nativeScalaCompiler` runs `snc` instead of the JVM Scala compiler, incrementally: Zinc recompiles what it would for `scalac`.
- **Live execution**: a program linked as a *host* can load code linked later against it, as an *extension* that shares its runtime, objects and classes. `snc-host` uses it to expand macros: it links a library's macro code against itself, compiles it with clang and loads it.
- **nscplugin for Scala 3.10.2 and later**: definitions grouped by owner (classes lost methods with newer compilers), and catch cases of union types (`case e: (A | B)`), which caught every subclass of the alternatives' common superclass.
- **A build for the Scala versions `snc` is built with**: the reference compiler 3.10.2-RC1 and the Scala 3 nightly `3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY`.

Everything else, javalib included, is upstream's.

## Building

You need what upstream needs: a JDK, sbt and clang/LLVM ([docs/user/setup.md](docs/user/setup.md)). Publish the artifacts that `snc` is built with and links against:

```sh
sbt -Dscalanative.ivyHome=$HOME/.ivy2-snc \
  "publish-local-dev 3.10.2-RC1" \
  "++3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY" \
  nscplugin3/publishLocal junitPlugin3/publishLocal scalalib3/publishLocal scala3lib3/publishLocal
```

`-Dscalanative.ivyHome=<dir>` (or `SCALANATIVE_IVY_HOME`) publishes into `<dir>/local` instead of `~/.ivy2/local`, so that these 0.5.13-SNAPSHOT artifacts do not replace others of the same version; build the Scala 3 fork with `-Dsbt.ivy.home=<dir>` then. Without it, everything goes to `~/.ivy2/local`.

## snlink

```sh
sbt -Dscalanative.ivyHome=$HOME/.ivy2-snc \
  'set LocalProject("snlink3") / nativeConfig ~= (_.withMode(scala.scalanative.build.Mode.releaseFast))' \
  snlink3/nativeLink
# target/out/native0.5/scala-3.10.0/snlink/snlink

snlink --main Main --base-dir target/link out/ <libraries>...
```

It takes the options of sbt's `nativeConfig` (`--mode`, `--gc`, `--lto`, `--multithreading`, `--embed-resources`, `--compile-option`, ...), `--build-target` for libraries, and the class path entries to link: the program's directories or jars, Scala Native's libraries (`nativelib`, `clib`, `posixlib`, `javalib`, `auxlib`, ...) and the Scala library's NIR (`scalalib_native0.5_3`, `scala3lib_native0.5_3`). `snlink --help` lists them all. `snlinkJVM3` is the same program on the JVM.

## Live execution

A host is linked with open and callable types: classes and traits that extensions may subclass or implement, and classes whose methods they may call. `snlink --open <pattern> --callable <pattern>` (or `NativeConfig.withLiveExecution`) links a host and writes its manifest, `<binary>.snm`, next to it:

```sh
snlink --main Host --open 'api.*' --open java.lang.Object --callable 'api.*' \
  --base-dir host host-classes/ <libraries>...
snlink --extension --host host/out.snm --ext ext-classes/ --module ext \
  --base-dir ext host-classes/ <libraries>...
```

The host loads the bundle with `scala.scalanative.runtime.Extension`. An extension may define classes that extend the host's open types and call what the host keeps callable; calls on open types stay virtual in the host. `--delta` and `--chain` link extensions against earlier ones, and `--stub-unlinkable` turns code that does not link (a JDK class javalib lacks, another platform's bindings) into stubs that throw `NoSuchMethodException` naming what is missing. Inside a program, `scala.scalanative.live.ExtensionLinker` links an extension on the calling thread without starting `snlink`. `scripts/live-execution/run.sh` and `chain.sh` link and run an example host with its extensions.

## snc, the native Scala 3 compiler

`snc` is built in the companion Scala 3 fork, from the artifacts above; its README describes it. It compiles Scala 3 to TASTy and NIR natively, against the Java API of an installed JDK, and `snc-host`, the same compiler linked as a live-execution host, expands macros.

## Compiling with snc in sbt

The sbt plugin compiles a project's `Compile` and `Test` sources with `snc` when `nativeScalaCompiler` names it:

```scala
// project/plugins.sbt
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.13-SNAPSHOT")

// build.sbt
ThisBuild / scalaVersion := "3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY"
ThisBuild / resolvers +=
  "scala-nightlies" at "https://repo.scala-lang.org/artifactory/maven-nightlies"
ThisBuild / nativeScalaCompiler := Some(file("/path/to/snc-host"))
ThisBuild / nativeScalaCompilerOptions := Seq()  // options for snc only, after scalacOptions
```

`publish-local-dev` above publishes the plugin for sbt 1; run sbt with `-Dsbt.ivy.home=$HOME/.ivy2-snc` to resolve it and the libraries from there. `nativeScalaCompiler` is `None` by default, which keeps the JVM compiler; a bare name such as `file("snc")` is looked up on the `PATH`.

- The Scala version should be the nightly that `snc` reports (`snc --about`), so that the libraries and the JVM compiler, which sbt still uses for `console` and `doc`, read the same TASTy.
- `scalacOptions`, the class path and nscplugin's `-Xplugin` go to `snc` as they go to `scalac` (`snc` maps the plugin jar to the plugin it has built in). `snc` compiles against the Java API of the JDK that sbt runs on, as `scalac` does; `--java-home <dir>` in `nativeScalaCompilerOptions` picks another.
- Macros expand only with `snc-host`; the plain `snc` fails on a macro call.
- Compilation is incremental: `snc --analysis` streams what its sbt phases report, and the plugin hands it to Zinc, which recompiles the sources it would recompile for `scalac`. With an `snc` that does not stream the analysis (`snc --about` does not say `"analysis": true`), any change of a source or of the class path recompiles the whole configuration.

## Tests

As upstream (`sbt tests3/test`, `sbt toolsJVM3/testFull`, ...). The live-execution tests are `LiveExecutionSpec`, `LiveChainSpec`, `LiveHostCodeGenTest`, `LiveCopiesTest` and `LiveHostRetentionTest` in `tools`.

---

The rest of this file is upstream's README.

# Scala Native

<!-- Disable unhelpful red badges until after Issue #4489 is resolved
![Posix Status](https://github.com/scala-native/scala-native/actions/workflows/run-tests-linux.yml/badge.svg)
![MacOS Status](https://github.com/scala-native/scala-native/actions/workflows/run-tests-macos.yml/badge.svg)
![Windows Status](https://github.com/scala-native/scala-native/actions/workflows/run-tests-windows.yml/badge.svg)
[![Maven Central](https://img.shields.io/maven-central/v/org.scala-native/tools_2.12.svg)](https://maven-badges.herokuapp.com/maven-central/org.scala-native/tools_2.12)
-->

Scala Native is an optimizing ahead-of-time compiler and lightweight managed runtime designed specifically for Scala.

## Documentation and Chat

Getting Started and full documentation can be found at [https://www.scala-native.org/](https://www.scala-native.org/).

Chat on the Scala Discord channel.
[![Discord](https://img.shields.io/discord/632150470000902164.svg?label=&logo=discord&logoColor=ffffff&color=404244&labelColor=6A7EC2)](https://discord.gg/scala)

## Online Scaladoc

[![Scaladoc nativelib](https://javadoc.io/badge2/org.scala-native/nativelib_native0.5_3/javadoc.svg?label=nativelib)](https://javadoc.io/doc/org.scala-native/nativelib_native0.5_3)
[![Scaladoc javalib](https://javadoc.io/badge2/org.scala-native/javalib_native0.5_2.13/javadoc.svg?label=javalib)](https://javadoc.io/doc/org.scala-native/javalib_native0.5_2.13)
[![Scaladoc clib](https://javadoc.io/badge2/org.scala-native/clib_native0.5_3/javadoc.svg?label=clib)](https://javadoc.io/doc/org.scala-native/clib_native0.5_3)
[![Scaladoc posixlib](https://javadoc.io/badge2/org.scala-native/posixlib_native0.5_3/javadoc.svg?label=posixlib)](https://javadoc.io/doc/org.scala-native/posixlib_native0.5_3)
[![Scaladoc windowslib](https://javadoc.io/badge2/org.scala-native/windowslib_native0.5_3/javadoc.svg?label=windowslib)](https://javadoc.io/doc/org.scala-native/windowslib_native0.5_3)

## License

Scala Native is distributed under the Apache License.
[See LICENSE.md for details](https://github.com/scala-native/scala-native/blob/main/LICENSE.md)

## Sponsors

[<img src="https://www.epfl.ch/wp/5.5/wp-content/themes/wp-theme-2018/assets/svg/epfl-logo.svg?refresh=now" width=200>][EPFL-Link]

[The École polytechnique fédérale de Lausanne (EPFL)][EPFL-Link] and [LAMP][EPFL_LAMP-Link] are funding the development of Scala Native as part of Martin Odersky's research program for [Capabilities for Resources and Effects (Caprese)][Caprese-Link] in Scala.


[<img src="https://raw.githubusercontent.com/VirtusLab/scala-cli/main/website/static/img/vl_logo_small.png" width=200>][VirtusLab-Link]

[VirtusLab][VirtusLab-Link] sponsors Scala Native by providing a full-time engineer designated to the maintenance and future development of Scala Native.

[<img src="https://raw.githubusercontent.com/scala-native/scala-native/main/docs/_static/logo.png" width=100>][ScalaNative-Link]

[Scala Native][ScalaNative-Link] is also powered by the efforts of its open-source community. Check out the list of [contributors][ScalaNative_contributors-Link] to this project.


[Caprese-Link]: https://www.slideshare.net/Odersky/capabilities-for-resources-and-effects-252161040
[EPFL-Link]: https://www.epfl.ch/en/
[EPFL_LAMP-Link]: https://www.epfl.ch/labs/lamp/
[VirtusLab-Link]: https://virtuslab.com/
[ScalaNative-Link]: https://scala-native.org/
[ScalaNative_contributors-Link]: https://github.com/scala-native/scala-native/graphs/contributors

