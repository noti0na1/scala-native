package scala.scalanative
package linker

import scalanative.util.Scope

object Link {

  /** Load all clases and methods reachable from the entry points. */
  def apply(config: build.Config, entries: Seq[nir.Global])(implicit
      scope: Scope
  ): ReachabilityAnalysis =
    Reach(config, entries, ClassLoader.fromDisk(config))

  /** Run reachability analysis on already loaded methods. */
  def apply(
      config: build.Config,
      entries: Seq[nir.Global],
      defns: Seq[nir.Defn]
  ): ReachabilityAnalysis =
    Reach(config, entries, ClassLoader.fromMemory(defns))

  /** Load all classes and methods reachable from the entry points with the
   *  given loader.
   */
  def withLoader(
      config: build.Config,
      entries: Seq[nir.Global],
      loader: ClassLoader
  ): ReachabilityAnalysis =
    Reach(config, entries, loader)

  /** Run reachability analysis on optimized methods of a host link. What
   *  extensions are compiled against, and what the host's optimized code
   *  assumes, stays as it was before optimization, also where the optimizer
   *  inlined or removed all uses of it: the classes, traits and modules, their
   *  fields, and which classes are allocated. Methods stay where they are
   *  reachable: an extension that calls one the host no longer has defines a
   *  copy of its own (`LiveReach.ExtensionContext.copiesMethod`).
   */
  def hostAfterOptimization(
      config: build.Config,
      before: ReachabilityAnalysis.Result,
      defns: Seq[nir.Defn]
  ): ReachabilityAnalysis = {
    val infos = before.infos.valuesIterator.toSeq
    val retained = Reach.Retained(
      scopes = infos.collect { case info: ScopeInfo => info.name }.sortBy(_.id),
      fields =
        infos.collect { case field: Field => field.name }.sortBy(_.mangle),
      allocated = infos
        .collect { case cls: Class if cls.allocated => cls.name }
        .sortBy(_.id)
    )
    Reach(config, before.entries, ClassLoader.fromMemory(defns), Some(retained))
  }

  /** Run reachability analysis on optimized methods of an extension link: the
   *  host's classes are read from the class path again, so that the host
   *  methods it reaches anew stay declarations of what the host defines, with
   *  the extension's optimized copies of the host methods the host does not
   *  export in place of their originals.
   */
  def extension(
      config: build.Config,
      entries: Seq[nir.Global],
      defns: Seq[nir.Defn],
      manifest: live.Manifest
  )(implicit scope: Scope): ReachabilityAnalysis =
    Reach(
      config,
      entries,
      new ClassLoader.Combined(
        ClassLoader.fromDisk(config),
        ClassLoader.fromMemory(defns),
        manifest.owns
      )
    )

}
