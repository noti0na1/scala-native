package scala.scalanative
package live

import scala.scalanative.linker.{
  Class, LinkingException, ReachabilityAnalysis, ScopeInfo, Trait
}

/** How an extension calls a virtual method on a receiver whose static type is a
 *  host class or trait: as the host would, since the extension does not know
 *  the host's other classes. Ids of slots and itable entries come from the
 *  manifest; a call the host devirtualized goes to the same target. A call the
 *  host does not dispatch, on a class whose subclasses the manifest lists and
 *  none of which is open, or on a trait no extension can implement, goes to the
 *  implementation of the receiver's class, which the extension's reachability
 *  found (`linker.LiveReach.resolveHostCall`).
 */
private[scalanative] sealed abstract class Dispatch

private[scalanative] object Dispatch {

  /** Straight to `target`; no target means a null receiver. */
  final case class Direct(target: Option[nir.Global.Member]) extends Dispatch

  /** Through the vtable of `cls`. */
  final case class Virtual(cls: Class) extends Dispatch

  /** Through the itable of `trt`. */
  final case class Interface(trt: Trait) extends Dispatch

  /** By the receiver's class: to the target of the case that lists its class,
   *  or of the last case.
   */
  final case class ByClass(cases: Seq[ClassCase]) extends Dispatch

  /** The host classes named `classes` run `target`. */
  final case class ClassCase(classes: Seq[String], target: nir.Global.Member)
}

private[scalanative] final class HostDispatch(
    val manifest: Manifest,
    objectClass: () => Class,
    extensionTargets: collection.Map[
      (nir.Global.Top, nir.Sig),
      Seq[Dispatch.ClassCase]
    ]
) {
  import Dispatch._

  def isHostOwned(name: nir.Global.Top): Boolean = manifest.owns(name)
  def isHostOwned(info: ScopeInfo): Boolean = manifest.owns(info.name)

  // Slots of a class: its parent's and its own
  private val slotSets =
    collection.concurrent.TrieMap.empty[String, Set[String]]
  private def slots(entry: Manifest.ClassEntry): Set[String] =
    slotSets.getOrElseUpdate(
      entry.name, {
        val inherited =
          manifest.classByName.get(entry.parent).fold(Set.empty[String])(slots)
        inherited ++ entry.vtable
      }
    )
  private lazy val objectSlots: Set[String] =
    manifest.classByName
      .get(nir.Rt.Object.name.id)
      .fold(Set.empty[String])(slots)
  private val methodSets =
    collection.concurrent.TrieMap.empty[String, Set[String]]
  private def methods(entry: Manifest.TraitEntry): Set[String] =
    methodSets.getOrElseUpdate(entry.name, entry.methods.toSet)

  private def target(symbol: String): Option[nir.Global.Member] =
    if (symbol.isEmpty) None
    else
      Some(nir.Unmangle.unmangleGlobal(symbol).asInstanceOf[nir.Global.Member])

  /** Whether calls on this host class may reach classes the extension does not
   *  know: subclasses in the host, or in other extensions.
   */
  def mayHaveUnknownSubclasses(cls: Class): Boolean =
    manifest.classByName.get(cls.name.id).exists { entry =>
      entry.isOpen || manifest.hasSubclasses.contains(entry.name)
    }

  def apply(scope: ScopeInfo, sig: nir.Sig): Dispatch =
    byHost(scope, sig)
      .orElse(extensionTargets.get((scope.name, sig)).map {
        case Seq(only) => Direct(Some(only.target))
        case cases     => ByClass(cases)
      })
      .getOrElse {
        throw new LinkingException(
          s"The extension calls ${sig.show} on host type ${scope.name.id}, " +
            "which the host never calls: add the type to the host's callable set"
        )
      }

  /** How the host dispatches the call, if it does. */
  def byHost(scope: ScopeInfo, sig: nir.Sig): Option[Dispatch] =
    // `Object`'s final methods (getClass, notify, wait): no class overrides
    // them, whatever the host knows about the receiver's type
    if (linker.LiveReach.isFinalMethod(nir.Rt.Object.name, sig))
      Some(Direct(Some(nir.Rt.Object.name.member(sig))))
    else own(scope, sig).orElse(throughSupertype(scope, sig))

  /** Through a host supertype that dispatches the method, as a callable type
   *  does every method it declares: its vtable slot, its itable entry or its
   *  recorded direct target serves every subtype.
   */
  private def throughSupertype(scope: ScopeInfo, sig: nir.Sig) =
    scope.linearized.iterator
      .drop(1)
      .filter(isHostOwned(_))
      .map {
        case cls: Class =>
          val entry = manifest.classByName(cls.name.id)
          if (slots(entry).contains(sig.mangle)) Some(Virtual(cls))
          else entry.directBySig.get(sig.mangle).map(s => Direct(target(s)))
        case trt: Trait =>
          val entry = manifest.traitByName(trt.name.id)
          if (methods(entry).contains(sig.mangle)) Some(Interface(trt))
          else entry.directBySig.get(sig.mangle).map(s => Direct(target(s)))
      }
      .collectFirst { case Some(dispatch) => dispatch }

  private def own(scope: ScopeInfo, sig: nir.Sig): Option[Dispatch] = {
    // A method no subclass can override resolves statically
    def final_(cls: Class): Option[Dispatch] =
      cls.resolve(sig).collect {
        case impl @ nir.Global.Member(owner, implSig)
            if implSig.isPrivate || implSig.isStatic ||
              linker.LiveReach.isFinalMethod(owner, implSig) =>
          Direct(Some(impl))
      }
    scope match {
      case cls: Class =>
        val entry = manifest.classByName(cls.name.id)
        if (slots(entry).contains(sig.mangle)) Some(Virtual(cls))
        else
          entry.directBySig.get(sig.mangle) match {
            case Some(symbol) => Some(Direct(target(symbol)))
            case None if !mayHaveUnknownSubclasses(cls) =>
              Some(Direct(cls.resolve(sig)))
            case None => final_(cls)
          }
      case trt: Trait =>
        val entry = manifest.traitByName(trt.name.id)
        entry.directBySig.get(sig.mangle) match {
          case Some(symbol) => Some(Direct(target(symbol)))
          case None         =>
            if (objectSlots.contains(sig.mangle))
              Some(Virtual(objectClass()))
            else if (methods(entry).contains(sig.mangle)) Some(Interface(trt))
            else final_(objectClass())
        }
    }
  }
}

private[scalanative] object HostDispatch {
  def apply(manifest: Manifest)(implicit
      analysis: ReachabilityAnalysis.Result
  ): HostDispatch =
    new HostDispatch(manifest, () => analysis.ObjectClass, analysis.hostTargets)
}
