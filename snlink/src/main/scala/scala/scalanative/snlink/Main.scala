package scala.scalanative.snlink

import java.nio.file.{Path, Paths}

import scala.scalanative.build._
import scala.scalanative.util.Scope

/** Links Scala Native programs without the JVM: the toolchain itself compiled
 *  to a native executable.
 *
 *  Usage: snlink --main <class> [options] <classpath entry>...
 *
 *  Run `snlink --help` for the options. Those not given take the defaults of
 *  sbt's `nativeConfig`; build mode, GC, LTO and optimization also fall back to
 *  the usual SCALANATIVE_MODE, SCALANATIVE_GC, SCALANATIVE_LTO and
 *  SCALANATIVE_OPTIMIZE environment variables, and clang is discovered as in
 *  sbt.
 */
object Main {
  private[snlink] case class Options(
      mainClass: Option[String] = None,
      baseDir: Path = Paths.get("target", "snlink").toAbsolutePath(),
      moduleName: String = "out",
      classPath: Vector[Path] = Vector.empty,
      mode: Option[Mode] = None,
      gc: Option[GC] = None,
      lto: Option[LTO] = None,
      optimize: Option[Boolean] = None,
      multithreading: Option[Boolean] = None,
      embedResources: Boolean = false,
      resourceIncludePatterns: Vector[String] = Vector.empty,
      resourceExcludePatterns: Vector[String] = Vector.empty,
      linkStubs: Boolean = false,
      check: Boolean = false,
      checkFatalWarnings: Boolean = false,
      debugInfo: Boolean = false,
      compileOptions: Vector[String] = Vector.empty,
      linkingOptions: Vector[String] = Vector.empty,
      targetTriple: Option[String] = None,
      serviceProviders: Map[String, Iterable[String]] = Map.empty,
      help: Boolean = false,
      version: Boolean = false,
      // Build target, tools, C and C++ options, logging (`toolchainOption`)
      buildTarget: BuildTarget = BuildTarget.default,
      clang: Option[Path] = None,
      clangPP: Option[Path] = None,
      cOptions: Vector[String] = Vector.empty,
      cppOptions: Vector[String] = Vector.empty,
      defaultCompileOptions: Boolean = true,
      defaultLinkingOptions: Boolean = true,
      logLevel: LogLevel = LogLevel.Info,
      // Live execution
      live: LiveOptions = LiveOptions()
  )

  private val usage =
    """usage: snlink --main <class> [options] <classpath entry>...
      |       snlink --build-target <library target> [options] <classpath entry>...
      |
      |  --base-dir <dir>              working and output directory (target/snlink)
      |  --module <name>               name of the output binary (out)
      |  --mode <mode>                 debug, release-fast, release-size or release-full
      |  --gc <gc>                     immix, commix, boehm or none
      |  --lto <lto>                   none, thin or full
      |  --optimize, --no-optimize     run the NIR optimizer (on)
      |  --multithreading <bool>       true or false; detected when absent
      |  --embed-resources             embed the class path's resources
      |  --resource-include <glob>     embed only matching resources (repeatable)
      |  --resource-exclude <glob>     do not embed matching resources (repeatable)
      |  --link-stubs                  link @stub definitions
      |  --check                       check the NIR, warnings not fatal
      |  --check-fatal-warnings        check the NIR, warnings fatal
      |  --debug-info                  emit source-level debug information
      |  --compile-option <option>     extra clang option for compiling (repeatable)
      |  --linking-option <option>     extra clang option for linking (repeatable)
      |  --target-triple <triple>      target other than the host
      |  --service-provider <service>=<impl>[,<impl>...]
      |                                implementations for ServiceLoader (repeatable)
      |
      |  --build-target <target>       application, library-dynamic or library-static
      |                                (application); libraries need no --main
      |  --clang <path>                clang instead of the discovered one
      |  --clang-pp <path>             clang++ instead of the discovered one
      |  --c-compile-option <option>   extra option for C sources only (repeatable)
      |  --cpp-compile-option <option> extra option for C++ sources only (repeatable)
      |  --no-default-compile-options  drop the discovered compile options
      |  --no-default-linking-options  drop the discovered linking options
      |  --log-level <level>           error, warn, info, debug or trace (info)
      |  -q, --quiet                   same as --log-level warn
      |  -v, --verbose                 same as --log-level debug
      |  --version                     print the Scala Native version and exit
      |  -h, --help                    print this help""".stripMargin +
      LiveOptions.usage

  def main(args: Array[String]): Unit = {
    val options =
      try parseArgs(args.toList)
      catch {
        case UsageError(message)         => exitWithUsage(message)
        case e: IllegalArgumentException => exitWithUsage(e.getMessage)
      }
    if (options.help) {
      println(usage)
      sys.exit(0)
    }
    if (options.version) {
      println(s"snlink ${scala.scalanative.nir.Versions.current}")
      sys.exit(0)
    }

    val logger = Main.logger(options.logLevel)
    exitOnBuildError(logger) {
      if (options.live.dumpManifest.nonEmpty || options.live.extension)
        runLive(options, logger)
      else link(options, logger)
    }
  }

  /** Runs `build`; a failed build (clang's errors are already logged) or link
   *  ends with its message as one error and exit code 1, as sbt reports it,
   *  instead of a stack trace.
   */
  private def exitOnBuildError(logger: Logger)(build: => Unit): Unit =
    try build
    catch {
      case e: BuildException =>
        logger.error(e.getMessage)
        sys.exit(1)
      case e: scala.scalanative.linker.LinkingException =>
        logger.error(e.getMessage)
        sys.exit(1)
    }

  private def link(options: Options, logger: Logger): Unit = {
    val config = Config.empty
      .withLogger(logger)
      .withBaseDir(options.baseDir)
      .withModuleName(options.moduleName)
      .withMainClass(options.mainClass)
      .withClassPath(options.classPath)
      .withCompilerConfig(nativeConfig(options, new Discovered(logger)))

    Scope { implicit scope =>
      val artifact = Build.buildCachedAwait(config)
      println(artifact)
    }
  }

  private[snlink] final case class UsageError(message: String)
      extends Exception(message)

  private def exitWithUsage(message: String): Nothing = {
    System.err.println(s"snlink: $message")
    System.err.println(usage)
    sys.exit(2)
  }

  private def fail(message: String): Nothing = throw UsageError(message)

  private def bool(value: String): Boolean = value match {
    case "true"  => true
    case "false" => false
    case _       => fail(s"expected true or false, got $value")
  }

  @annotation.tailrec
  private def parse(args: List[String], acc: Options): Options = args match {
    case "--main" :: cls :: rest => parse(rest, acc.copy(mainClass = Some(cls)))
    case "--base-dir" :: dir :: rest =>
      parse(rest, acc.copy(baseDir = Paths.get(dir).toAbsolutePath()))
    case "--module" :: name :: rest => parse(rest, acc.copy(moduleName = name))
    case "--mode" :: mode :: rest   =>
      parse(rest, acc.copy(mode = Some(Mode(mode))))
    case "--gc" :: gc :: rest    => parse(rest, acc.copy(gc = Some(GC(gc))))
    case "--lto" :: lto :: rest  => parse(rest, acc.copy(lto = Some(LTO(lto))))
    case "--optimize" :: rest    => parse(rest, acc.copy(optimize = Some(true)))
    case "--no-optimize" :: rest =>
      parse(rest, acc.copy(optimize = Some(false)))
    case "--multithreading" :: value :: rest =>
      parse(rest, acc.copy(multithreading = Some(bool(value))))
    case "--embed-resources" :: rest =>
      parse(rest, acc.copy(embedResources = true))
    case "--resource-include" :: glob :: rest =>
      parse(
        rest,
        acc.copy(resourceIncludePatterns = acc.resourceIncludePatterns :+ glob)
      )
    case "--resource-exclude" :: glob :: rest =>
      parse(
        rest,
        acc.copy(resourceExcludePatterns = acc.resourceExcludePatterns :+ glob)
      )
    case "--link-stubs" :: rest => parse(rest, acc.copy(linkStubs = true))
    case "--check" :: rest      => parse(rest, acc.copy(check = true))
    case "--check-fatal-warnings" :: rest =>
      parse(rest, acc.copy(checkFatalWarnings = true))
    case "--debug-info" :: rest => parse(rest, acc.copy(debugInfo = true))
    case "--compile-option" :: option :: rest =>
      parse(rest, acc.copy(compileOptions = acc.compileOptions :+ option))
    case "--linking-option" :: option :: rest =>
      parse(rest, acc.copy(linkingOptions = acc.linkingOptions :+ option))
    case "--target-triple" :: triple :: rest =>
      parse(rest, acc.copy(targetTriple = Some(triple)))
    case "--service-provider" :: spec :: rest =>
      val (service, impls) = spec.split("=", 2) match {
        case Array(service, impls) => (service, impls.split(",").toSeq)
        case _                     => (spec, Nil)
      }
      val providers = acc.serviceProviders.getOrElse(service, Nil) ++ impls
      parse(
        rest,
        acc.copy(serviceProviders =
          acc.serviceProviders + (service -> providers)
        )
      )
    case LiveOptions.Parse(live, rest) =>
      parse(rest, acc.copy(live = live(acc.live)))
    case ("-h" | "--help") :: _ => acc.copy(help = true)
    case "--version" :: _       => acc.copy(version = true)
    case option :: rest if toolchainOptions.contains(option) =>
      val (remaining, updated) = toolchainOption(option, rest, acc)
      parse(remaining, updated)
    case option :: _ if option.startsWith("-") =>
      fail(s"unknown option $option")
    case entry :: rest =>
      parse(
        rest,
        acc.copy(classPath = acc.classPath :+ Paths.get(entry).toAbsolutePath())
      )
    case Nil => acc
  }

  /** The options `args` give, or a `UsageError`. */
  private[snlink] def parseArgs(args: List[String]): Options = {
    val options = parse(args, Options())
    val live = options.live
    if (options.help || options.version || live.dumpManifest.nonEmpty)
      options
    else if (live.extension) {
      if (live.hostManifest.isEmpty) fail("--extension needs --host <manifest>")
      else if (live.roots.isEmpty)
        fail("--extension needs at least one --ext entry")
      else options
    } else if (options.classPath.isEmpty) fail("no class path entries")
    else if (options.mainClass.isEmpty &&
        options.buildTarget == BuildTarget.application)
      fail("--main is required to link an application")
    else options
  }

  // Build target, tools, C and C++ options, logging
  private val toolchainOptions = Set(
    "--build-target",
    "--clang",
    "--clang-pp",
    "--c-compile-option",
    "--cpp-compile-option",
    "--no-default-compile-options",
    "--no-default-linking-options",
    "--log-level",
    "-q",
    "--quiet",
    "-v",
    "--verbose"
  )

  /** Parses one of `toolchainOptions`; returns the arguments left. */
  private def toolchainOption(
      option: String,
      rest: List[String],
      acc: Options
  ): (List[String], Options) = {
    def withValue(update: String => Options): (List[String], Options) =
      rest match {
        case value :: remaining => (remaining, update(value))
        case Nil                => fail(s"$option needs a value")
      }
    def path(value: String) = Some(Paths.get(value).toAbsolutePath())
    option match {
      case "--build-target" =>
        withValue {
          case "application" =>
            acc.copy(buildTarget = BuildTarget.application)
          case "library-dynamic" =>
            acc.copy(buildTarget = BuildTarget.libraryDynamic)
          case "library-static" =>
            acc.copy(buildTarget = BuildTarget.libraryStatic)
          case other =>
            fail(
              s"unknown build target $other, expected application, library-dynamic or library-static"
            )
        }
      case "--clang"    => withValue(value => acc.copy(clang = path(value)))
      case "--clang-pp" => withValue(value => acc.copy(clangPP = path(value)))
      case "--c-compile-option" =>
        withValue(value => acc.copy(cOptions = acc.cOptions :+ value))
      case "--cpp-compile-option" =>
        withValue(value => acc.copy(cppOptions = acc.cppOptions :+ value))
      case "--no-default-compile-options" =>
        (rest, acc.copy(defaultCompileOptions = false))
      case "--no-default-linking-options" =>
        (rest, acc.copy(defaultLinkingOptions = false))
      case "--log-level" =>
        withValue { value =>
          val level = LogLevel.all.find(_.name == value).getOrElse {
            fail(
              s"unknown log level $value, expected error, warn, info, debug or trace"
            )
          }
          acc.copy(logLevel = level)
        }
      case "-q" | "--quiet"   => (rest, acc.copy(logLevel = LogLevel.Warn))
      case "-v" | "--verbose" => (rest, acc.copy(logLevel = LogLevel.Debug))
    }
  }

  private[snlink] sealed abstract class LogLevel(
      val name: String,
      val verbosity: Int
  )
  private[snlink] object LogLevel {
    case object Error extends LogLevel("error", 0)
    case object Warn extends LogLevel("warn", 1)
    case object Info extends LogLevel("info", 2)
    case object Debug extends LogLevel("debug", 3)
    case object Trace extends LogLevel("trace", 4)
    val all: List[LogLevel] = List(Error, Warn, Info, Debug, Trace)
  }

  /** `underlying` without the messages more verbose than `level`. */
  private[snlink] def logger(
      level: LogLevel,
      underlying: Logger = Logger.default
  ): Logger = {
    def shows(at: LogLevel) = at.verbosity <= level.verbosity
    Logger(
      traceFn = t => if (shows(LogLevel.Trace)) underlying.trace(t),
      debugFn = m => if (shows(LogLevel.Debug)) underlying.debug(m),
      infoFn = m => if (shows(LogLevel.Info)) underlying.info(m),
      warnFn = m => if (shows(LogLevel.Warn)) underlying.warn(m),
      errorFn = m => underlying.error(m)
    )
  }

  /** Where the settings the options leave out come from: discovery, as for
   *  sbt's `nativeConfig`. Tests replace it.
   */
  private[snlink] trait Defaults {
    def clang(): Path
    def clangPP(): Path
    def compileOptions(): Seq[String]
    def linkingOptions(): Seq[String]
    def mode(): Mode
    def gc(): GC
    def lto(): LTO
    def optimize(): Boolean
  }

  /** `Discover`, with clang's version warnings going to `logger`. */
  private final class Discovered(logger: Logger) extends Defaults {
    private def discoverClang(binary: String): Path = {
      val path = Discover.discover(binary, "LLVM_BIN")
      Discover.checkClangVersion(path, logger)
      path
    }
    def clang(): Path = discoverClang("clang")
    def clangPP(): Path = discoverClang("clang++")
    def compileOptions(): Seq[String] = Discover.compileOptions()
    def linkingOptions(): Seq[String] = Discover.linkingOptions()
    def mode(): Mode = Discover.mode()
    def gc(): GC = Discover.GC()
    def lto(): LTO = Discover.LTO()
    def optimize(): Boolean = Discover.optimize()
  }

  private[snlink] def nativeConfig(
      options: Options,
      defaults: Defaults
  ): NativeConfig = {
    val compileOptions =
      if (options.defaultCompileOptions) defaults.compileOptions() else Nil
    val linkingOptions =
      if (options.defaultLinkingOptions) defaults.linkingOptions() else Nil
    NativeConfig.empty
      .withClang(options.clang.getOrElse(defaults.clang()))
      .withClangPP(options.clangPP.getOrElse(defaults.clangPP()))
      .withCompileOptions(compileOptions ++ options.compileOptions)
      .withCOptions(options.cOptions)
      .withCppOptions(options.cppOptions)
      .withLinkingOptions(linkingOptions ++ options.linkingOptions)
      .withBuildTarget(options.buildTarget)
      .withMode(options.mode.getOrElse(defaults.mode()))
      .withGC(options.gc.getOrElse(defaults.gc()))
      .withLTO(options.lto.getOrElse(defaults.lto()))
      .withOptimize(options.optimize.getOrElse(defaults.optimize()))
      .withMultithreading(options.multithreading)
      .withEmbedResources(options.embedResources)
      .withResourceIncludePatterns(
        if (options.resourceIncludePatterns.isEmpty)
          NativeConfig.empty.resourceIncludePatterns
        else options.resourceIncludePatterns
      )
      .withResourceExcludePatterns(
        NativeConfig.empty.resourceExcludePatterns ++
          options.resourceExcludePatterns
      )
      .withLinkStubs(options.linkStubs)
      .withCheck(options.check || options.checkFatalWarnings)
      .withCheckFatalWarnings(options.checkFatalWarnings)
      .withSourceLevelDebuggingConfig(_.enabled(options.debugInfo))
      .withTargetTriple(options.targetTriple)
      .withServiceProviders(options.serviceProviders)
      .withLiveExecution(options.live.host)
  }

  // ---------------------------------------------------------------------------
  // Live execution: host links with open and callable types, and extension
  // links against a host's manifest. Kept apart from the options above.
  // ---------------------------------------------------------------------------

  private[snlink] case class LiveOptions(
      open: Vector[String] = Vector.empty,
      callable: Vector[String] = Vector.empty,
      extension: Boolean = false,
      hostManifest: Option[Path] = None,
      hostBinary: Option[Path] = None,
      roots: Vector[Path] = Vector.empty,
      dumpManifest: Option[Path] = None,
      // Chained extensions and hosts whose boundary is everything (the REPL)
      chain: Vector[Path] = Vector.empty,
      delta: Option[Path] = None,
      interruptHandler: Option[String] = None,
      stubUnlinkable: Boolean = false
  ) {
    def host: LiveExecution =
      if (open.isEmpty && callable.isEmpty) LiveExecution.Disabled
      else
        LiveExecution.Host(
          open = open,
          callable = callable,
          stubUnlinkable = stubUnlinkable
        )
  }

  private[snlink] object LiveOptions {
    val usage =
      """|
         |
         |live execution, host:
         |  --open <pattern>              classes and traits extensions may subclass or
         |                                implement (repeatable; `pkg.*` for a prefix)
         |  --callable <pattern>          classes kept callable by extensions with all
         |                                their methods (repeatable)
         |
         |live execution, extension:
         |  snlink --extension --host <manifest> --ext <entry>... [options] <classpath entry>...
         |  --extension                   link an extension bundle against a host
         |  --host <manifest>             the host's manifest (<host binary>.snm)
         |  --host-binary <path>          the host executable (from the manifest if absent)
         |  --ext <entry>                 class path entry whose classes make up the
         |                                extension (repeatable)
         |  --dump-manifest <manifest>    print a manifest and exit
         |
         |live execution, chained extensions and hosts whose boundary is everything:
         |  --stub-unlinkable             host or extension: methods whose code does not
         |                                link become stubs that throw, instead of
         |                                failing the link
         |  --chain <delta>               extension: link against an earlier extension's
         |                                delta too (repeatable, in load order)
         |  --delta <path>                extension: write this extension's delta, so
         |                                that later extensions can link against it
         |  --interrupt-handler <module>.<method>
         |                                extension: poll the host's interrupt flag at
         |                                method entries and loop back-edges, calling
         |                                this host method while it is set""".stripMargin

    object Parse {
      def unapply(
          args: List[String]
      ): Option[(LiveOptions => LiveOptions, List[String])] = args match {
        case "--open" :: p :: rest =>
          Some((o => o.copy(open = o.open :+ p), rest))
        case "--callable" :: p :: rest =>
          Some((o => o.copy(callable = o.callable :+ p), rest))
        case "--extension" :: rest =>
          Some((o => o.copy(extension = true), rest))
        case "--host" :: m :: rest =>
          Some((o => o.copy(hostManifest = Some(abs(m))), rest))
        case "--host-binary" :: b :: rest =>
          Some((o => o.copy(hostBinary = Some(abs(b))), rest))
        case "--ext" :: e :: rest =>
          Some((o => o.copy(roots = o.roots :+ abs(e)), rest))
        case "--dump-manifest" :: m :: rest =>
          Some((o => o.copy(dumpManifest = Some(abs(m))), rest))
        case "--stub-unlinkable" :: rest =>
          Some((o => o.copy(stubUnlinkable = true), rest))
        case "--chain" :: d :: rest =>
          Some((o => o.copy(chain = o.chain :+ abs(d)), rest))
        case "--delta" :: d :: rest =>
          Some((o => o.copy(delta = Some(abs(d))), rest))
        case "--interrupt-handler" :: h :: rest =>
          Some((o => o.copy(interruptHandler = Some(h)), rest))
        case _ => None
      }
    }

    private def abs(p: String): Path = Paths.get(p).toAbsolutePath()
  }

  /** Prints a manifest, or links an extension: `parseArgs` checked that an
   *  extension link has `--host` and `--ext`. The GC and multithreading come
   *  from the host's manifest, and so does the mode unless `--mode` is given;
   *  the discovered default linking options do not apply to an extension.
   */
  private def runLive(options: Options, logger: Logger): Unit = {
    val live = options.live
    live.dumpManifest match {
      case Some(path) =>
        print(scala.scalanative.live.Manifest.read(path).show)
      case None =>
        val config = extensionConfig(options, logger, new Discovered(logger))
        Scope { implicit scope =>
          val artifact = Build.buildCachedAwait(config)
          println(artifact)
        }
    }
  }

  /** The configuration of an extension link. */
  private[snlink] def extensionConfig(
      options: Options,
      logger: Logger,
      defaults: Defaults
  ): Config = {
    val live = options.live
    val manifestPath = live.hostManifest.get
    val manifest = scala.scalanative.live.Manifest.read(manifestPath)
    val compileOptions =
      if (options.defaultCompileOptions) defaults.compileOptions() else Nil
    val nativeConfig = NativeConfig.empty
      .withClang(options.clang.getOrElse(defaults.clang()))
      .withClangPP(options.clangPP.getOrElse(defaults.clangPP()))
      .withCompileOptions(compileOptions ++ options.compileOptions)
      .withCOptions(options.cOptions)
      .withCppOptions(options.cppOptions)
      .withLinkingOptions(options.linkingOptions)
      .withMode(options.mode.getOrElse(Mode(manifest.header.mode)))
      .withGC(GC(manifest.header.gc))
      .withOptimize(options.optimize.getOrElse(defaults.optimize()))
      .withMultithreading(manifest.header.multithreading)
      .withCheck(options.check || options.checkFatalWarnings)
      .withCheckFatalWarnings(options.checkFatalWarnings)
      .withSourceLevelDebuggingConfig(_.enabled(options.debugInfo))
      .withTargetTriple(options.targetTriple)
      .withLiveExecution(
        LiveExecution.Extension(
          hostManifest = manifestPath,
          roots = live.roots,
          hostBinary = live.hostBinary,
          chain = live.chain,
          delta = live.delta,
          interruptHandler = live.interruptHandler,
          stubUnlinkable = live.stubUnlinkable
        )
      )
    Config.empty
      .withLogger(logger)
      .withBaseDir(options.baseDir)
      .withModuleName(options.moduleName)
      .withClassPath((live.roots ++ options.classPath).distinct)
      .withCompilerConfig(nativeConfig)
  }
}
