# Scala Native, with a native toolchain and live execution

This is a fork of [Scala Native](https://github.com/scala-native/scala-native) (based on its `main`) with what a Scala 3 compiler built with Scala Native needs: [snc](#snc-the-native-scala-3-compiler), whose sources are in a companion fork of Scala 3. It adds:

- **`snlink`**, the Scala Native toolchain as a native command-line linker: it links Scala Native programs and libraries without a JVM.
- **Compiling with `snc` in sbt and Mill**: the sbt plugin's `nativeScalaCompiler`, and the Mill plugin's `SncModule`, compile with `snc` instead of the JVM Scala compiler, incrementally: Zinc recompiles what it would for `scalac`. [examples/](examples) has a project for each.
- **Live execution**: a program linked as a *host* can load code linked later against it, as an *extension* that shares its runtime, objects and classes. `snc-host` uses it to expand macros: it links a library's macro code against itself, compiles it with clang and loads it.
- **nscplugin fixes**: catch cases of union types (`case e: (A | B)`), which with Scala 3.10.2 and later caught every subclass of the alternatives' common superclass; no `java.util.Locale`, which javalib lacks, so that the plugin can be built into `snc`; and definitions grouped by top-level name rather than by output file, without which classes lost their methods with the Scala 3.10.1 release candidates (their `PlainFile`s for the same file were not equal).
- **A build for the Scala versions `snc` is built with**: the reference compiler 3.10.2-RC1 and the Scala 3 nightly `3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY`.

Everything else, javalib included, is upstream's.

## Building and publishing locally

You need what upstream needs: a JDK, sbt and clang/LLVM ([docs/user/setup.md](docs/user/setup.md)); for the Mill plugin, Mill 1.3. Everything is published to the local Ivy repository, `~/.ivy2/local`, where sbt, Mill and the Scala 3 fork's build look.

1. The artifacts that `snc` is built with and that programs compiled by it link against, and the sbt plugin (for sbt 1):

   ```sh
   sbt "publish-local-dev 3.10.2-RC1" \
     "++3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY" \
     nscplugin3/publishLocal junitPlugin3/publishLocal scalalib3/publishLocal scala3lib3/publishLocal
   ```

2. For Mill: the toolchain built for Scala 2.13, which Mill's Scala Native support loads, then the Mill plugin, which [mill-scala-native/](mill-scala-native) builds with Mill:

   ```sh
   sbt utilJVM2_13/publishLocal nirJVM2_13/publishLocal toolsJVM2_13/publishLocal testRunner2_13/publishLocal
   cd mill-scala-native && mill __.publishLocal
   ```

3. `snc`: build it in the companion Scala 3 fork, as its README describes. Programs that call macros, such as the [examples](#compiling-with-snc-in-sbt-and-mill), need `snc-host`.

The version of these artifacts is 0.5.13-SNAPSHOT, the version that upstream's own local builds publish. To keep them apart, use another Ivy home `<dir>` throughout:

- this repository's `sbt` commands: `-Dscalanative.ivyHome=<dir>` (or `SCALANATIVE_IVY_HOME=<dir>`);
- the Scala 3 fork's build and sbt projects: `sbt -Dsbt.ivy.home=<dir>`;
- Mill, the plugin's build included: `export COURSIER_REPOSITORIES="ivy:file://<dir>/local/[defaultPattern]|central"`, which takes `<dir>/local` instead of `~/.ivy2/local`, and `mill __.publishLocal --localIvyRepo <dir>/local` to publish the plugin.

## snlink

```sh
sbt 'set LocalProject("snlink3") / nativeConfig ~= (_.withMode(scala.scalanative.build.Mode.releaseFast))' \
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

## Compiling with snc in sbt and Mill

[examples/sbt](examples/sbt) and [examples/mill](examples/mill) are the same project for each build tool: a library, an application that uses it and os-lib, and their MUnit tests, all compiled by `snc`. They need only the published artifacts and `snc-host`, which expands the macros of os-lib and MUnit, so they can be copied anywhere. Name `snc-host` in `SNC`, or put it on the `PATH` as `snc`:

```sh
export SNC=/path/to/snc-host

cd examples/sbt
sbt app/run test

cd ../mill
mill app.run
mill __.test
```

`app/run` prints:

```
Hello, Scala Native!
n=8 mean=3.875 median=4 max=9
```

### sbt

```scala
// project/plugins.sbt
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.5.13-SNAPSHOT")

// build.sbt
ThisBuild / scalaVersion := "3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY"
ThisBuild / resolvers +=
  "scala-nightlies" at "https://repo.scala-lang.org/artifactory/maven-nightlies"
ThisBuild / nativeScalaCompiler := Some(file(sys.env.getOrElse("SNC", "snc")))
ThisBuild / nativeScalaCompilerOptions := Seq()  // options for snc only, after scalacOptions
```

The plugin, published for sbt 1, compiles the `Compile` and `Test` sources of Scala Native projects with the `snc` that `nativeScalaCompiler` names. `None`, the default, keeps the JVM compiler; a bare name such as `file("snc")` is looked up on the `PATH`.

### Mill

```scala
//| mill-version: 1.3.1
//| mvnDeps: ["org.scala-native::mill-scala-native-snc::0.5.13-SNAPSHOT"]
package build

import mill.*
import mill.scalalib.*
import mill.scalanativelib.snc.SncModule

object app extends SncModule {
  def scalaVersion = "3.11.0-RC1-bin-20261119-90f3daf-NIGHTLY"
  def scalaNativeVersion = "0.5.13-SNAPSHOT"
  override def repositories = Task {
    super.repositories() :+ "https://repo.scala-lang.org/artifactory/maven-nightlies"
  }
  object test extends SncTests with TestModule.Munit {
    def munitVersion = "1.2.1"
  }
}
```

`SncModule`, for Mill 1.3, is Mill's `ScalaNativeModule` with a `compile` that runs `snc`: `nativeScalaCompiler`, by default `$SNC`, else `snc` on the `PATH`, with `nativeScalaCompilerOptions` after the Scala compiler options. Linking, `run` and `test` are Mill's. `snc` writes no class files, so main classes and tests are found from Zinc's analysis, and `SncTests` runs the tests in the linked test binary without loading them on the JVM.

### In both

- The Scala version should be the nightly that `snc` reports (`snc --about`), the version that the Scala Native libraries above are published for.
- The Scala compiler options, the class path and nscplugin's `-Xplugin` go to `snc` as they go to `scalac` (`snc` maps the plugin jar to the plugin it has built in). `snc` compiles against the Java API of the JDK that `scalac` would: the one sbt runs on, or the Mill module's `javaHome`, else the one Mill runs on. `--java-home <dir>` in `nativeScalaCompilerOptions` picks another.
- Macros expand only with `snc-host`; the plain `snc` fails on a macro call.

### Incremental compilation

`snc --analysis` streams what its sbt phases report, and the plugins hand it to Zinc, which recompiles the sources that it would recompile for `scalac`. In the examples, after a first build (`sbt Test/compile`, `mill __.compile`):

| Edit | Recompiled |
|---|---|
| The body of `greet` in `lib`'s `Greeting.scala` | `Greeting.scala` |
| An overload, `def greet(name: String, times: Int): String = greet(name) * times`, next to it | `Greeting.scala`, `app`'s `Main.scala`, and both test suites, which call `greet` |
| The body of `mean` in `app`'s `Stats.scala` | `Stats.scala` |
| Nothing | nothing |

sbt and Mill print what they compile: `compiling 1 Scala source to .../lib/target/...`, `lib.compile compiling 1 Scala source to out/lib/...`. Mill turns Zinc's incremental compilation off for modules with one source file (`zincIncrementalCompilation`), such as the examples' test modules: they recompile whenever a module they depend on changes, with `scalac` as with `snc`. With an `snc` that does not stream the analysis (`snc --about` does not say `"analysis":true`), any change of a source or of the class path recompiles everything.

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

