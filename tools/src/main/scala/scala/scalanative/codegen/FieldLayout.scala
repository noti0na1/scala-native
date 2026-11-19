package scala.scalanative
package codegen

import scala.scalanative.build.BuildException
import scalanative.linker.{Class, Field}

private[codegen] class FieldLayout(cls: Class)(implicit meta: Metadata) {

  import meta.layouts.{ArrayHeader, Object, ObjectHeader}
  import meta.platform

  def index(fld: Field) = entries.indexOf(fld) + Object.ValuesOffset
  // Proxy fields due to cyclic dependency
  def entries: Seq[Field] = entries0
  def layout: MemoryLayout = layout0

  // A host class in an extension link: its own fields in the host's order
  private def hostFieldOrder: Option[Seq[String]] = meta.live match {
    case ext: LiveMetadata.Extension if ext.isHostOwned(cls) =>
      Some(ext.manifestClass(cls).fields.map(_.name))
    case _ => None
  }

  private def ownFields: Seq[Field] = {
    val own = cls.members.collect { case f: Field => f }.toSeq
    hostFieldOrder.fold(own) { order =>
      val byName = own.map(f => f.name.mangle -> f).toMap
      own.foreach { f =>
        if (!order.contains(f.name.mangle))
          throw new BuildException(
            s"The extension uses field ${f.name.show}, which the host does not keep"
          )
      }
      order.map { name =>
        byName.getOrElse(
          name,
          throw new BuildException(
            s"Field $name of host class ${cls.name.id} was not loaded"
          )
        )
      }
    }
  }

  private lazy val (entries0, layout0) = {
    val entries: Seq[Field] = {
      val base = cls.parent.fold {
        Seq.empty[Field]
      } { parent => meta.layout(parent).entries }
      base ++ ownFields
    }
    val usesCustomAlignment = entries.exists(_.attrs.align.isDefined)
    val isArray = nir.Type.isArray(cls.name)
    if (usesCustomAlignment) {
      assert(!isArray) // Only regular object can have custom alignmet
      val fields = entries.sortBy(_.attrs.align.flatMap(_.group))
      val layout = MemoryLayout.ofAlignedFields(fields)
      (fields, layout)
    } else {
      val rttiHeader = if (isArray) ArrayHeader else ObjectHeader
      val layout = MemoryLayout(rttiHeader.layout +: entries.map(_.ty))
      (entries, layout)
    }
  }

  val struct = nir.Type.StructValue(layout.tys.map(_.ty))
  val size = layout.size
  /* Reference map used by the GC: offsets of reference fields, -1, then
   * offsets of raw pointer fields to scan conservatively, -1. Only boxed
   * pointers (unsafe.Ptr) scan raw pointers: one can point into the heap, for
   * example to an array element, and be the only thing keeping it alive.
   * Weak references leave out their referent, which the GC does not mark but
   * clears once nothing else keeps it alive.
   */
  val referenceOffsetsValue = {
    val conservative =
      if (cls.name == nir.Rt.BoxedPtr.name) layout.rawPointerFieldsOffsets
      else Nil
    val referent = referentOffset
    val weakReferent =
      if (meta.referenceKind(cls).contains(FieldLayout.WeakReference))
        referent
      else None
    val strong = layout.referenceFieldsOffsets.filterNot(weakReferent.contains)
    val offsets = (strong :+ -1) ++ (conservative :+ -1)
    nir.Val.Const(nir.Val.ArrayValue(nir.Type.Int, offsets.map(nir.Val.Int(_))))
  }

  /* Offset of the referent of java.lang.ref.Reference. The GC finds it right
   * after the object header, like the fields of String and Class.
   */
  private def referentOffset: Option[Int] =
    entries.find(_.name == FieldLayout.Referent).map { fld =>
      val offset = layout.tys(index(fld)).offset.toInt
      val expected = MemoryLayout.sizeOf(ObjectHeader.layout).toInt
      if (offset != expected)
        throw new build.BuildException(
          s"The referent of ${cls.name.id} must follow the object header at offset $expected, found at $offset"
        )
      offset
    }

  // The extension's view of a host class must match the host's
  meta.live match {
    case ext: LiveMetadata.Extension if ext.isHostOwned(cls) =>
      val expected = ext.manifestClass(cls).size
      if (expected != size)
        throw new BuildException(
          s"Layout of host class ${cls.name.id}: the host has $expected bytes, the extension computes $size"
        )
    case _ => ()
  }

}

private[codegen] object FieldLayout {
  val Reference = nir.Global.Top("java.lang.ref.Reference")
  val WeakReference = nir.Global.Top("java.lang.ref.WeakReference")
  val SoftReference = nir.Global.Top("java.lang.ref.SoftReference")
  val PhantomReference = nir.Global.Top("java.lang.ref.PhantomReference")
  // The first field of every reference object, see java.lang.ref.Reference
  val Referent: nir.Global.Member =
    Reference.member(nir.Sig.Field("_gc_modified_referent"))
}
