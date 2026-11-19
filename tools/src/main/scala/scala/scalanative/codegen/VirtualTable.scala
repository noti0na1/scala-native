package scala.scalanative
package codegen

import scala.collection.mutable

private[codegen] class VirtualTable(cls: linker.Class)(implicit
    meta: Metadata
) {

  // A host class in an extension link takes its slots from the host: its
  // parent's, then those its manifest entry adds
  private val hostSlots: Option[Seq[nir.Sig]] = meta.live match {
    case ext: LiveMetadata.Extension if ext.isHostOwned(cls) =>
      val inherited =
        cls.parent.fold(Seq.empty[nir.Sig])(meta.vtable(_).signatures)
      Some(inherited ++ ext.manifestClass(cls).vtable.map(new nir.Sig(_)))
    case _ => None
  }

  private val slots: mutable.UnrolledBuffer[nir.Sig] =
    hostSlots.fold {
      cls.parent.fold {
        mutable.UnrolledBuffer.empty[nir.Sig]
      } { parent => meta.vtable(parent).slots.clone }
    } { slots => mutable.UnrolledBuffer.empty[nir.Sig] ++= slots }
  private val impls: mutable.Map[nir.Sig, nir.Val] =
    mutable.Map.empty[nir.Sig, nir.Val]
  locally {
    def addSlot(sig: nir.Sig): Unit = {
      assert(!slots.contains(sig))
      val index = slots.size
      slots += sig
    }
    def addImpl(sig: nir.Sig): Unit = {
      val impl =
        cls
          .resolve(sig)
          .map(nir.Val.Global(_, nir.Type.Ptr))
          .getOrElse(meta.live.missingImplementation(cls, sig))
      impls(sig) = impl
    }
    if (hostSlots.isDefined) {
      // Never emitted: only its slot indexes and type are used
      slots.foreach(impls(_) = nir.Val.Null)
    } else {
      val hasPhantoms = meta.analysis.phantoms.nonEmpty
      // A phantom target stands for extension code, and an instance may
      // lack an implementation in a host link: dispatch is virtual
      def needsSlot(sig: nir.Sig) = {
        val targets = cls.targets(sig)
        targets.size > 1 ||
          (hasPhantoms && targets.exists(meta.analysis.isPhantom)) ||
          meta.live.lacksImplementations(cls, sig)
      }
      slots.foreach { sig => addImpl(sig) }
      // Sorted: slot order must not follow hash order of calls
      cls.calls.toSeq
        .filter(sig => !impls.contains(sig) && needsSlot(sig))
        .sortBy(_.mangle)
        .foreach { sig =>
          addSlot(sig)
          addImpl(sig)
        }
    }
  }
  val value: nir.Val =
    nir.Val.ArrayValue(nir.Type.Ptr, slots.map(impls).toSeq)
  val ty =
    value.ty
  def index(sig: nir.Sig): Int =
    slots.indexOf(sig)
  def at(index: Int): nir.Val =
    impls(slots(index))

  /** Signatures in slot order. */
  def signatures: Seq[nir.Sig] = slots.toSeq

}
