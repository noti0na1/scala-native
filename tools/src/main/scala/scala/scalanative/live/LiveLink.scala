package scala.scalanative
package live

import scala.scalanative.build.{BuildException, Config, LiveExecution}
import scala.scalanative.linker.{ClassLoader, Link, ReachabilityAnalysis}
import scala.scalanative.util.Scope

/** Reachability roots for live-execution links. */
private[scalanative] object LiveLink {

  /** A host link: the entries, plus every class matching the callable patterns
   *  and all of its methods. Callable classes whose closure does not link (a
   *  missing JDK class, an unsupported feature) are dropped and reachability
   *  runs again, so the boundary holds the callable classes that link.
   */
  def host(config: Config, entries: Seq[nir.Global], host: LiveExecution.Host)(
      implicit scope: Scope
  ): ReachabilityAnalysis = {
    val loader = ClassLoader.fromDisk(config)
    if (host.callable.isEmpty) Link.withLoader(config, entries, loader)
    else {
      val callable = loader.classes.toSeq
        .filter(top => host.isCallable(top.id))
        .sortBy(_.id)
      var roots: Map[nir.Global.Top, Seq[nir.Global]] =
        callable.map { top =>
          val methods = loader.load(top).toSeq.flatten.collect {
            case defn: nir.Defn.Define => defn.name
          }
          top -> (top +: methods.sortBy(_.mangle))
        }.toMap
      def attempt() =
        Link.withLoader(
          config,
          entries ++ roots.toSeq.sortBy(_._1.id).flatMap(_._2),
          loader
        )
      var result = attempt()
      var rounds = 1
      var dropped = List.empty[nir.Global.Top]
      while (!result.isSuccessful && rounds < 50) {
        val failure = result.asInstanceOf[ReachabilityAnalysis.Failure]
        val culprits: Set[nir.Global.Top] =
          (failure.unreachable.flatMap(u =>
            u.name.top :: u.backtrace.map(_.name.top)
          ) ++ failure.unsupportedFeatures.flatMap(
            _.backtrace.map(_.name.top)
          )).toSet
        val before = roots.size
        dropped ++= roots.keys.filter(culprits.contains)
        roots = roots.filterNot { case (top, _) => culprits.contains(top) }
        if (roots.size == before) return result
        result = attempt()
        rounds += 1
      }
      if (dropped.nonEmpty)
        config.logger.info(
          s"Live execution: ${dropped.size} callable classes do not link and stay out of the boundary: " +
            dropped.map(_.id).sorted.take(20).mkString(", ") +
            (if (dropped.size > 20) ", ..." else "")
        )
      result
    }
  }

  /** An extension link's roots: every class in its own class path entries, with
   *  all of its methods. Not a NIR file that defines no class (the companion
   *  class the compiler dropped, whose file holds only static lazy-val offset
   *  accessors), nor an extern object, whose C functions code that calls them
   *  reaches, as in a host link (`HostStubs`).
   */
  def extensionEntries(config: Config, ext: LiveExecution.Extension)(implicit
      scope: Scope
  ): Seq[nir.Global] = {
    val manifest = ManifestChain.manifest(ext)
    val own = ext.roots.flatMap { path =>
      val cp = linker.ClassPath(io.VirtualDirectory.real(path), config.logger)
      cp.classes.toSeq.sortBy(_.id).map(top => top -> cp.load(top))
    }
    own.flatMap {
      case (top, _) if manifest.owns(top) =>
        // The host, or an earlier extension of the chain, defines it, and a
        // class has a single definition
        Nil
      case (top, defns) =>
        val all = defns.toSeq.flatten
        val defined = all.exists {
          case d: nir.Defn.Class  => d.name == top && !d.attrs.isExtern
          case d: nir.Defn.Module => d.name == top && !d.attrs.isExtern
          case d: nir.Defn.Trait  => d.name == top && !d.attrs.isExtern
          case _                  => false
        }
        if (!defined) Nil
        else top +: all.collect { case defn: nir.Defn.Define => defn.name }
    } ++ objectFinalMethods(manifest)
  }

  /** `Object`'s final methods the host exports (`getClass`, ...): calls of them
   *  on host types go straight to them (`HostDispatch`), so they are declared
   *  whether reachability reached them or not.
   */
  private def objectFinalMethods(manifest: Manifest): Seq[nir.Global] = {
    val owner = nir.Rt.Object.name.id
    val prefix = s"M${owner.length}$owner"
    manifest.exports.iterator
      .filter(_.startsWith(prefix))
      .map(nir.Unmangle.unmangleGlobal)
      .collect {
        case m @ nir.Global.Member(owner, sig)
            if linker.LiveReach.isFinalMethod(owner, sig) =>
          m
      }
      .toSeq
  }

  /** Checks an extension's configuration against the host's manifest. */
  def checkExtensionConfig(config: Config, manifest: Manifest): Unit = {
    val h = manifest.header
    def fail(msg: String) =
      throw new BuildException(s"Extension does not match its host: $msg")
    if (h.runtimeAbi != Manifest.RuntimeAbiVersion)
      fail(
        s"host runtime ABI ${h.runtimeAbi}, this linker ${Manifest.RuntimeAbiVersion}"
      )
    if (h.nirCompat != nir.Versions.compat)
      fail(
        s"host NIR compat ${h.nirCompat}, this linker ${nir.Versions.compat}"
      )
    if (h.gc != config.gc.name)
      fail(s"host GC ${h.gc}, extension ${config.gc.name}")
    if (config.compilerConfig.multithreading.exists(_ != h.multithreading))
      fail(
        s"host multithreading ${h.multithreading}, extension ${config.compilerConfig.multithreading.get}"
      )
    if (config.compilerConfig.lto != build.LTO.None)
      fail("extensions cannot use LTO")
  }
}
