package scala.scalanative
package live

import java.nio.file.{Files, Path, Paths}

import scala.collection.mutable
import scala.concurrent.ExecutionContext

import scala.scalanative.build._
import scala.scalanative.util.Scope

/** The extension link of `snlink --extension` as a library, for a program that
 *  links extensions without starting `snlink`. For example, a compiler linked
 *  as a host links macro code against itself and loads it with
 *  `scala.scalanative.runtime.Extension`, and its REPL links lines against the
 *  evaluator's manifest.
 *
 *  Links run one at a time, on the calling thread, and start no threads of
 *  their own: in a single-threaded program, clang runs with its output sent to
 *  files (`build.Processes`). Between links, the linker keeps the class path's
 *  jars with the NIR read from them (`linker.ClassPathCache`), the manifests
 *  and their chains, and the toolchain.
 *
 *  @param toolchain
 *    clang and the options its compiles and links take
 */
final class ExtensionLinker(val toolchain: ExtensionLinker.Toolchain) {
  import ExtensionLinker._

  // Jars stay open between links, for the life of the program
  linker.ClassPathCache.enable(Scope.forever)

  /** Links the classes of `request.roots` into a bundle that the host can load;
   *  its path.
   *
   *  @throws build.BuildException
   *    if the extension does not link: it subclasses a type the host does not
   *    declare open, it needs a symbol the host lacks, or clang fails
   */
  def link(request: Request): Path = {
    val (config, ext) = configure(request)
    // Every task runs at once on this thread, so the link is over when
    // buildExtension returns
    Scope { implicit scope =>
      Build.buildExtension(config, ext)(scope, CallingThread).value match {
        case Some(result) => result.get
        case None         =>
          throw new IllegalStateException("the extension link did not finish")
      }
    }
  }

  /** The reachability analysis of a link of `request`, without compiling: reads
   *  the NIR of the classes it reaches.
   */
  private def reach(request: Request): Unit = {
    val (config, ext) = configure(request)
    Scope { implicit scope =>
      Build.reachExtensionOnly(config, ext)(scope, CallingThread).value match {
        case Some(result) => result.get
        case None         =>
          throw new IllegalStateException("the extension link did not finish")
      }
    }
  }

  /** The configuration of a link of `request`, with its host's settings. */
  private def configure(request: Request): (Config, LiveExecution.Extension) = {
    val header = Manifest.read(request.host.manifest).header
    val nativeConfig = NativeConfig.empty
      .withClang(toolchain.clang)
      .withClangPP(toolchain.clangPP)
      .withCompileOptions(toolchain.compileOptions ++ request.compileOptions)
      .withLinkingOptions(toolchain.linkingOptions)
      .withTargetTriple(header.targetTriple)
      .withMode(request.mode.getOrElse(Mode(header.mode)))
      .withGC(GC(header.gc))
      .withOptimize(request.optimize)
      .withMultithreading(header.multithreading)
      .withLiveExecution(
        LiveExecution.Extension(
          hostManifest = request.host.manifest,
          roots = request.roots,
          hostBinary = Some(request.host.binary),
          chain = request.chain,
          delta = request.delta,
          stubUnlinkable = request.stubUnlinkable
        )
      )
    val config = Config.empty
      .withLogger(request.logger)
      .withBaseDir(request.workDir)
      .withModuleName(request.moduleName)
      // The host's classes from the NIR the host was linked from, before any
      // other copy of them
      .withClassPath(
        (request.roots ++ request.host.classPath ++ request.classPath).distinct
      )
      .withCompilerConfig(nativeConfig)
    Files.createDirectories(config.workDir)
    val ext = config.compilerConfig.liveExecution match {
      case ext: LiveExecution.Extension => ext
      case _ => throw new IllegalStateException("not an extension link")
    }
    (config, ext)
  }

  /** Reads what a link of `args` (`snlink`'s arguments of an extension link)
   *  needs before it is made, one step per element, each named by the element:
   *  the host's manifest and the chain, then their lookups, then each jar of
   *  the class path, listed and then searched for entry points, and last the
   *  reachability analysis of the request, which reads the NIR of the classes
   *  every extension uses (clang does not run). A single-threaded program can
   *  take one step at a time while it waits for its user: none takes much more
   *  than a tenth of a second. A request that does not parse has no steps.
   */
  def prepare(args: Seq[String]): Iterator[String] =
    Request.parse(args, Logger.nullLogger) match {
      case Left(_)        => Iterator.empty
      case Right(request) =>
        def step(name: String)(body: => Unit): () => String =
          () => { body; name }
        val ext = LiveExecution.Extension(
          hostManifest = request.host.manifest,
          roots = request.roots,
          chain = request.chain
        )
        val manifest = Iterator(
          step("manifest") {
            Manifest.read(request.host.manifest)
            request.chain.foreach(Manifest.read)
          },
          step("manifest lookups") {
            val m = ManifestChain.manifest(ext)
            m.classByName; m.traitByName; m.exportSet; m.hasSubclasses
            ManifestChain(ext).hostHash
          }
        )
        val jars =
          (request.roots ++ request.host.classPath ++ request.classPath).distinct
            .filter(p => Files.isRegularFile(p))
        def open(jar: Path) =
          linker.ClassPathCache.lookup(jar, Logger.nullLogger)
        val listed = jars.iterator.map { jar =>
          step(jar.getFileName.toString)(open(jar))
        }
        val searched = jars.iterator.map { jar =>
          step(s"${jar.getFileName} entry points") {
            open(jar).foreach { cp =>
              cp.classesWithEntryPoints; cp.definedServicesProviders
            }
          }
        }
        val reached = Iterator(step("classes") {
          try reach(request)
          catch { case _: Exception => () }
        })
        (manifest ++ listed ++ searched ++ reached).map(_.apply())
    }

  /** Links a request given as `snlink`'s arguments of an extension link
   *  (`--extension --host <manifest> --ext <entry> ... <class path>`). Returns
   *  the artifact, if the link succeeds, and the log, with the timings of its
   *  phases (`[info] Linking (...) (12 ms)`).
   */
  def link(args: Seq[String]): Response = {
    val log = mutable.ArrayBuffer.empty[String]
    def record(level: String)(msg: String): Unit =
      msg.split('\n').foreach(l => log += s"[$level] $l")
    val logger = Logger(
      traceFn = _ => (),
      debugFn = record("debug"),
      infoFn = record("info"),
      warnFn = record("warn"),
      errorFn = record("error")
    )
    Request.parse(args, logger) match {
      case Left(msg) =>
        record("error")(msg)
        Response(None, log.toList)
      case Right(request) =>
        try Response(Some(link(request)), log.toList)
        catch {
          case e: BuildException =>
            record("error")(e.getMessage)
            Response(None, log.toList)
          case e: linker.LinkingException =>
            record("error")(e.getMessage)
            Response(None, log.toList)
        }
    }
  }
}

object ExtensionLinker {

  /** A host: its manifest (`<binary>.snm`), its executable, and the NIR it was
   *  linked from (the class path a request names may also hold it).
   */
  final case class Host(manifest: Path, binary: Path, classPath: Seq[Path])

  /** clang and the options of compiles and links (`build.Discover`'s for the
   *  machine, as `snlink` uses).
   */
  final case class Toolchain(
      clang: Path,
      clangPP: Path,
      compileOptions: Seq[String],
      linkingOptions: Seq[String]
  )

  object Toolchain {

    /** The machine's clang and default options, as `snlink` finds them. */
    def discover(): Toolchain =
      Toolchain(
        clang = Discover.clang(),
        clangPP = Discover.clangpp(),
        compileOptions = Discover.compileOptions(),
        linkingOptions = Discover.linkingOptions()
      )
  }

  /** One extension link.
   *
   *  @param host
   *    the host it links against
   *  @param roots
   *    NIR directories or jars whose classes make up the extension
   *  @param workDir
   *    where the link writes its IR and objects, and the bundle
   *    (`<moduleName>.bundle` on macOS, `.so` elsewhere)
   *  @param moduleName
   *    the bundle's name
   *  @param classPath
   *    NIR the extension's classes need besides the host's
   *  @param mode
   *    the build mode, the host's by default
   *  @param optimize
   *    whether to run the optimizer
   *  @param chain
   *    deltas of the extensions it links against (REPL lines)
   *  @param delta
   *    where to write its own delta, so that later extensions link against it
   *  @param stubUnlinkable
   *    whether methods whose code does not link become stubs that throw
   *    (`live.HostStubs.linkExtension`), instead of the link failing
   */
  final case class Request(
      host: Host,
      roots: Seq[Path],
      workDir: Path,
      moduleName: String,
      classPath: Seq[Path] = Nil,
      mode: Option[Mode] = None,
      optimize: Boolean = true,
      chain: Seq[Path] = Nil,
      delta: Option[Path] = None,
      compileOptions: Seq[String] = Nil,
      logger: Logger = Logger.default,
      stubUnlinkable: Boolean = false
  )

  object Request {

    /** A request from `snlink --extension` arguments: `--host`,
     *  `--host-binary`, `--ext`, `--chain`, `--delta`, `--stub-unlinkable`,
     *  `--base-dir`, `--module`, `--mode`, `--optimize`, `--no-optimize`,
     *  `--compile-option` and class path entries; `--extension` is implied.
     */
    def parse(args: Seq[String], logger: Logger): Either[String, Request] = {
      var manifest: Option[Path] = None
      var binary: Option[Path] = None
      val roots = mutable.ArrayBuffer.empty[Path]
      val chain = mutable.ArrayBuffer.empty[Path]
      var delta: Option[Path] = None
      var baseDir: Path = Paths.get(".").toAbsolutePath()
      var module = "out"
      var mode: Option[Mode] = None
      var optimize = true
      var stubUnlinkable = false
      val compileOptions = mutable.ArrayBuffer.empty[String]
      val classPath = mutable.ArrayBuffer.empty[Path]
      def path(p: String) = Paths.get(p).toAbsolutePath()
      @scala.annotation.tailrec
      def loop(args: List[String]): Option[String] = args match {
        case Nil                   => None
        case "--extension" :: rest => loop(rest)
        case "--host" :: m :: rest => manifest = Some(path(m)); loop(rest)
        case "--host-binary" :: b :: rest => binary = Some(path(b)); loop(rest)
        case "--ext" :: e :: rest         => roots += path(e); loop(rest)
        case "--chain" :: d :: rest       => chain += path(d); loop(rest)
        case "--delta" :: d :: rest       => delta = Some(path(d)); loop(rest)
        case "--stub-unlinkable" :: rest  => stubUnlinkable = true; loop(rest)
        case "--base-dir" :: d :: rest    => baseDir = path(d); loop(rest)
        case "--module" :: m :: rest      => module = m; loop(rest)
        case "--mode" :: m :: rest        =>
          mode = scala.util.Try(Mode(m)).toOption
          if (mode.isEmpty) Some(s"unknown mode $m") else loop(rest)
        case "--optimize" :: rest            => optimize = true; loop(rest)
        case "--no-optimize" :: rest         => optimize = false; loop(rest)
        case "--compile-option" :: o :: rest =>
          compileOptions += o; loop(rest)
        // What the host and the linker decide: the host's threading and
        // GC, the linker's log level
        case ("--multithreading" | "--gc" | "--lto" |
            "--log-level") :: _ :: rest =>
          loop(rest)
        case ("-v" | "-q" | "--verbose" | "--quiet" | "--check" |
            "--check-fatal-warnings" | "--debug-info") :: rest =>
          loop(rest)
        case opt :: _ if opt.startsWith("-") =>
          Some(s"unknown option of an extension link: $opt")
        case entry :: rest => classPath += path(entry); loop(rest)
      }
      loop(args.toList) match {
        case Some(error) => Left(error)
        case None        =>
          manifest match {
            case None => Left("an extension link needs --host <manifest>")
            case _ if roots.isEmpty =>
              Left("an extension link needs --ext <entry>")
            case Some(m) =>
              val bin = binary.getOrElse(
                Paths.get(Manifest.read(m).header.hostBinary)
              )
              Right(
                Request(
                  host = Host(m, bin, Nil),
                  roots = roots.toList,
                  workDir = baseDir,
                  moduleName = module,
                  classPath = classPath.toList,
                  mode = mode,
                  optimize = optimize,
                  chain = chain.toList,
                  delta = delta,
                  compileOptions = compileOptions.toList,
                  logger = logger,
                  stubUnlinkable = stubUnlinkable
                )
              )
          }
      }
    }
  }

  /** What a link request gives: the bundle, or none, and the log. */
  final case class Response(artifact: Option[Path], log: Seq[String])

  /** Runs every task at once on the calling thread. */
  private object CallingThread extends ExecutionContext {
    def execute(task: Runnable): Unit = task.run()
    def reportFailure(cause: Throwable): Unit = cause.printStackTrace()
  }
}
