package mill.scalanativelib.snc

import mill.*
import mill.api.{BuildCtx, PathRef}
import mill.javalib.api.CompilationResult
import mill.javalib.testrunner.{TestResult, TestRunnerUtils}
import mill.javalib.{Dep, TestModule}
import mill.scalanativelib.ScalaNativeModule

/** A Scala Native module whose Scala sources are compiled by `snc`, the native
 *  Scala 3 compiler, instead of the JVM Scala compiler.
 *
 *  {{{
 *  object app extends SncModule {
 *    def scalaVersion = "<the version that snc --about reports>"
 *    def scalaNativeVersion = "0.5.13-SNAPSHOT"
 *    object test extends SncTests with TestModule.Munit
 *  }
 *  }}}
 *
 *  Compilation is incremental: Zinc runs `snc` (in the plugin's worker, with
 *  the sbt plugin's `SncCompiler`), reads the analysis that `snc --analysis`
 *  streams and recompiles the sources it would recompile for `scalac`. The
 *  analysis is stored where and as Mill stores its own, so modules compiled by
 *  Mill's JVM compiler can depend on this one. The output holds TASTy and NIR,
 *  and no class files, which is all that dependent modules and linking need;
 *  main classes and tests are found from the analysis.
 */
trait SncModule extends ScalaNativeModule { outer =>

  /** A test module compiled by `snc`. Its tests are found from the analysis and
   *  run in the linked test binary, as for any Scala Native test module.
   */
  trait SncTests extends ScalaNativeTests with SncModule {
    override def nativeScalaCompiler: T[PathRef] =
      Task { outer.nativeScalaCompiler() }
    override def nativeScalaCompilerOptions: T[Seq[String]] =
      Task { outer.nativeScalaCompilerOptions() }

    override protected def discoverTestsWithZinc: Boolean = true

    /** Runs the tests that Zinc's analysis finds, without loading them on the
     *  JVM: Mill's own test task loads each test class from the class files
     *  that `scalac` writes and `snc` does not.
     */
    override protected def testTask(
        args: Task[Seq[String]],
        globSelectors: Task[Seq[String]]
    ): Task[(msg: String, results: Seq[TestResult])] = Task.Anon {
      val discovered = zincAnalysisFileDiscoveredTestClasses()
      val selected = TestRunnerUtils.globFilter(globSelectors())
      val testArgs = args()
      val reportXml = testReportXml()
      val reporter = Task.testReporter
      val ctx = Task.ctx()
      val (close, framework) =
        withScalaNativeBridge
          .apply()
          .apply(
            _.getFramework(
              nativeLink().path.toIO,
              allForkEnv(),
              toWorkerApi(logLevel()),
              testFramework()
            )
          )
      try {
        val fingerprints = framework.fingerprints()
        val defs = discovered.collect {
          case (name, index) if selected(name) =>
            new sbt.testing.TaskDef(
              name.stripSuffix("$"),
              fingerprints(index),
              false,
              Array(new sbt.testing.SuiteSelector)
            )
        }
        val runner = framework.runner(
          testArgs.toArray,
          Array.empty[String],
          getClass.getClassLoader
        )
        val grouped = runner
          .tasks(defs.toArray)
          .toSeq
          .groupBy(t => Option(t.taskDef()).map(_.fullyQualifiedName()))
          .toSeq
          .sortBy(_._1)
          .map(_._2)
        val (doneMsg, results) =
          TestRunnerUtils.runTasks(grouped, reporter, runner, None)
        TestModule.handleResults(doneMsg, results.toSeq, ctx, reportXml)
      } finally {
        // Let the test binary finish writing its output, as Mill does.
        Thread.sleep(100)
        close()
      }
    }
  }

  /** The `snc` executable: `$SNC`, else `snc` on the `PATH`. Use `snc-host` for
   *  code that calls macros, the plain `snc` does not expand them.
   */
  def nativeScalaCompiler: T[PathRef] = Task.Input {
    SncModule.locate(Task.env)
  }

  /** Options for `snc` only, after the Scala compiler options. */
  def nativeScalaCompilerOptions: T[Seq[String]] = Task { Seq.empty[String] }

  /** What goes on `snc`'s command line after the Scala compiler options: the
   *  JDK whose Java API `scalac` would compile against (`javaHome`, else the
   *  JDK that Mill runs on), unless [[nativeScalaCompilerOptions]] names one
   *  with `--java-home`, then those options.
   */
  def sncOptions: T[Seq[String]] = Task {
    val options = nativeScalaCompilerOptions()
    val namesJdk =
      options.exists(o => o == "--java-home" || o.startsWith("--java-home="))
    val jdk = javaHome().map(_.path.toString).getOrElse(sys.props("java.home"))
    (if (namesJdk) Nil else Seq("--java-home", jdk)) ++ options
  }

  /** The worker's class path, resolved as Mill resolves its own workers. */
  def sncWorkerClasspath: T[Seq[PathRef]] = Task {
    jvmWorker()
      .defaultResolver()
      .classpath(Seq(Dep.parse(SncPluginBuildInfo.worker)))
  }

  private def sncWorker = Task.Anon { SncWorker(sncWorkerClasspath()) }

  /** Compiles the module's Scala sources with Zinc around `snc`: the classes in
   *  `compile.dest/classes`, the analysis in `compile.dest/zinc`, problems
   *  printed as Mill prints `scalac`'s and passed on to the task's reporter.
   */
  override def compile: T[CompilationResult] = Task(persistent = true) {
    val (scalaSources, others) =
      allSourceFiles().map(_.path).partition(_.ext == "scala")
    if (others.nonEmpty)
      Task.log.warn(
        s"snc compiles Scala sources only; ignoring ${others.size} other file(s)"
      )
    val classpath = compileClasspath()
      .map(_.path)
      .filter(p => os.exists(p) && p.ext != "pom")
    val options = allScalacOptions()

    val classes = Task.dest / "classes"
    val analysisFile = Task.dest / "zinc"
    val log = Task.log
    val colored = log.prompt.colored
    // As Mill does for scalac: the compiler colours its messages unless told
    // not to. Zinc's setup comparison ignores `-color:` options.
    val colorOption =
      if (colored || options.exists(_.startsWith("-color:"))) Nil
      else Seq("-color:never")
    val reporter = Task.reporter.apply(hashCode)
    reporter.foreach(_.start())
    val printer = new MillReporter(
      log,
      colored,
      BuildCtx.workspaceRoot,
      reporter
        .map(_.maxErrors)
        .getOrElse(
          mill.api.daemon.internal.CompileProblemReporter.defaultMaxErrors
        ),
      reporter
    )
    val failure =
      try
        sncWorker().compile(
          snc = nativeScalaCompiler().path,
          sncOptions = sncOptions(),
          workingDirectory = BuildCtx.workspaceRoot,
          scalaVersion = scalaVersion(),
          sources = scalaSources,
          classpath = classpath,
          options = colorOption ++ options,
          classes = classes,
          analysisFile = analysisFile,
          upstream = upstreamCompileOutput()
            .map(r => r.classes.path -> r.analysisFile),
          incremental = zincIncrementalCompilation(),
          reportCachedProblems = zincReportCachedProblems(),
          debugEnabled = log.debugEnabled,
          log = printer.message,
          problem = printer.problem,
          summary = () => printer.printSummary()
        )
      finally
        reporter.foreach { r =>
          scalaSources.foreach(s => r.fileVisited(s.toNIO))
          r.finish()
        }
    failure.foreach(message => Task.fail(message))
    CompilationResult(analysisFile, PathRef(classes))
  }

  /** The main classes that Zinc's analysis records: Mill's own discovery reads
   *  class files, which `snc` does not write.
   */
  override def allLocalMainClasses: T[Seq[String]] = Task {
    sncWorker().mainClasses(compile().analysisFile)
  }
}

object SncModule {

  /** `$SNC`, else `snc` on the `PATH`. The reference is quick (size and
   *  modification time): the binary is large and changes rarely.
   */
  def locate(env: Map[String, String]): PathRef = {
    def onPath: Option[os.Path] =
      env
        .getOrElse("PATH", "")
        .split(java.io.File.pathSeparatorChar)
        .iterator
        .filter(_.nonEmpty)
        .map(dir => os.Path(dir, os.pwd) / "snc")
        .find(os.isFile(_))
    env.get("SNC").map(os.Path(_, os.pwd)).orElse(onPath) match {
      case Some(p) if os.isFile(p) => PathRef(p, quick = true)
      case Some(p) => throw new Exception(s"SNC names $p, which is not a file")
      case None    =>
        throw new Exception("Cannot find snc: set SNC or put snc on the PATH")
    }
  }
}
