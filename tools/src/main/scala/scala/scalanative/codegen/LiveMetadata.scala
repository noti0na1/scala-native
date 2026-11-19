package scala.scalanative
package codegen

import scala.collection.mutable

import scala.scalanative.build.{BuildException, LiveExecution}
import scala.scalanative.linker.{Class, ScopeInfo, Trait}
import scala.scalanative.live.Manifest

/** How live execution changes metadata.
 *
 *  RTTI depends only on a type and its ancestors, so the classes of an
 *  extension need no ids from the host: their lineages and itables refer to the
 *  host's RTTI where they extend or implement host types. What does depend on
 *  the rest of the program is how code dispatches: vtable slots, the order of a
 *  trait's methods in itables, field layouts, and the facts about itables that
 *  lowering takes for granted for the whole program. Host links keep those
 *  facts out of the code that may meet instances of extension classes;
 *  extension links take the host's slots, method orders and layouts from its
 *  manifest and assume nothing about the itables of the host's classes.
 */
private[scalanative] sealed abstract class LiveMetadata {
  def isHostOwned(info: ScopeInfo): Boolean = false
  def isExtension: Boolean = false
  def isHost: Boolean = false

  /** Whether this link knows the itables of every class that implements `trt`,
   *  so that lowering may rely on where they store it
   *  (`Metadata.alwaysAtHomeSlot`, `maxDisplacement`, `neverShifted`). Not in a
   *  host link for a trait that extension classes may implement, and not in an
   *  extension link, which does not know the host's classes' itables.
   */
  def knowsItables(trt: Trait): Boolean = true

  /** The dispatch table entry of `cls` for a method it has no implementation
   *  of.
   */
  def missingImplementation(cls: Class, sig: nir.Sig): nir.Val = nir.Val.Null

  /** Whether an instance on which code calls `sig` as a `scope` may have no
   *  implementation of it: the call then goes through a dispatch table.
   */
  def lacksImplementations(scope: ScopeInfo, sig: nir.Sig): Boolean = false

  /** Whether a method this link defines is a copy of a method of the host or of
   *  an earlier extension (`linker.LiveReach.ExtensionContext.copiesMethod`):
   *  hidden, as the class's link defines the method's symbol, if anywhere.
   */
  def isCopy(name: nir.Global): Boolean = false
}

private[scalanative] object LiveMetadata {

  def apply(meta: Metadata): LiveMetadata =
    meta.config.liveExecution match {
      case LiveExecution.Disabled       => Disabled
      case host: LiveExecution.Host     => new Host(host, meta)
      case ext: LiveExecution.Extension =>
        new Extension(ext, live.ManifestChain(ext), meta)
    }

  object Disabled extends LiveMetadata

  final class Host(val config: LiveExecution.Host, meta: Metadata)
      extends LiveMetadata {
    val openClasses: Set[Class] =
      meta.analysis.openScopes.collect { case cls: Class => cls }.toSet
    val openTraits: Set[Trait] =
      meta.analysis.openScopes.collect { case trt: Trait => trt }.toSet
    override def isHost: Boolean = true

    // Traits a phantom implements: the open traits, their parents and the
    // traits of open classes, which extension classes may implement
    private lazy val phantomTraits: Set[Trait] =
      meta.allTraits.iterator
        .filter(_.implementors.exists(c => meta.analysis.isPhantom(c.name)))
        .toSet
    override def knowsItables(trt: Trait): Boolean =
      !phantomTraits.contains(trt)
    // A host link keeps no implementation of a method that only made-up calls
    // ask for in a class outside its boundary (`linker.LiveReach.makeUpCall`),
    // and leaves out methods that do not link (`live.HostStubs`). Extensions
    // may still meet an instance of such a class through a boundary type. Its
    // dispatch table entries for those methods are stubs that throw
    // `NoSuchMethodException` naming the method, one per signature, and calls
    // that may reach it dispatch through the tables, not to a single target.
    private val stubs = mutable.LinkedHashMap.empty[nir.Sig, nir.Global.Member]

    private def lacks(cls: Class, sig: nir.Sig): Boolean =
      cls.allocated && !meta.analysis.isPhantom(cls.name) &&
        cls.resolve(sig).isEmpty

    override def missingImplementation(cls: Class, sig: nir.Sig): nir.Val =
      if (!lacks(cls, sig)) nir.Val.Null
      else
        nir.Val.Global(
          stubs.getOrElseUpdate(
            sig,
            HostStubOwner.member(nir.Sig.Generated(sig.mangle))
          ),
          nir.Type.Ptr
        )

    override def lacksImplementations(scope: ScopeInfo, sig: nir.Sig): Boolean =
      scope match {
        case cls: Class =>
          lacks(cls, sig) || cls.subclasses.exists(lacks(_, sig))
        case trt: Trait => trt.implementors.exists(lacks(_, sig))
      }

    /** The stubs that the dispatch tables name, once they are built. */
    def stubDefns: Seq[nir.Defn.Define] =
      stubs.toSeq.map { case (sig, name) => stub(sig, name) }

    private def stub(sig: nir.Sig, name: nir.Global.Member): nir.Defn.Define = {
      implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
      implicit val fresh: nir.Fresh = nir.Fresh()
      implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
      def show(ty: nir.Type): String = ty match {
        case ref: nir.Type.RefKind => ref.className.id
        case _                     => ty.show
      }
      val (params, result, method) = sig.unmangled match {
        case nir.Sig.Method(id, types, _) =>
          (
            types.init,
            types.last,
            types.init.map(show).mkString(id + "(", ", ", ")")
          )
        case _ => (Nil, nir.Type.Nothing, sig.mangle)
      }
      val ty = nir.Type.Function(nir.Rt.Object +: params, result)
      val message =
        s"$method is not linked for this object: its class is outside the " +
          "live-execution boundary of the host (its open and callable classes)"
      nir.Defn.Define(
        nir.Attrs.None,
        name,
        ty,
        Seq(
          nir.Inst.Label(fresh(), ty.args.map(nir.Val.Local(fresh(), _))),
          nir.Inst.Let(
            nir.Op.Call(
              Lower.throwNoSuchMethodStringTy,
              Lower.throwNoSuchMethodVal,
              Seq(nir.Val.Null, nir.Val.String(message))
            ),
            nir.Next.None
          ),
          nir.Inst.Unreachable(nir.Next.None)
        )
      )
    }
  }

  /** The owner of the stubs in host dispatch tables (`Host.stubDefns`). */
  val HostStubOwner = nir.Global.Top("__scalanative_host_stub")

  final class Extension(
      val config: LiveExecution.Extension,
      val chain: live.ManifestChain,
      meta: Metadata
  ) extends LiveMetadata {
    private implicit def analysis: linker.ReachabilityAnalysis.Result =
      meta.analysis

    /** The host's manifest, with the classes of the extensions this one is
     *  chained to (`live.ManifestChain`).
     */
    val manifest: Manifest = chain.merged

    /** The hash of the host's manifest, which the loader checks. */
    def hostHash: String = chain.hostHash

    val dispatch = live.HostDispatch(manifest)

    override def isExtension: Boolean = true
    override def knowsItables(trt: Trait): Boolean = false
    override def isHostOwned(info: ScopeInfo): Boolean =
      manifest.owns(info.name)
    override def isCopy(name: nir.Global): Boolean = name match {
      case nir.Global.Member(owner, _) => manifest.owns(owner)
      case _                           => false
    }

    def manifestClass(cls: Class): Manifest.ClassEntry =
      manifest.classByName.getOrElse(
        cls.name.id,
        throw new BuildException(s"${cls.name.id} is not in the host manifest")
      )

    def manifestTrait(trt: Trait): Manifest.TraitEntry =
      manifest.traitByName.getOrElse(
        trt.name.id,
        throw new BuildException(s"${trt.name.id} is not in the host manifest")
      )

    def hostTraitMethods(trt: Trait): Seq[nir.Sig] =
      manifestTrait(trt).methods.map(new nir.Sig(_))
  }
}
