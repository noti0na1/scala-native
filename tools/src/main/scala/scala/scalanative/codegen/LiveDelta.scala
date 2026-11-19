package scala.scalanative
package codegen

import scala.scalanative.linker.{Class, Field, ScopeInfo, Trait}
import scala.scalanative.live.Manifest

/** Writes a chained extension's delta (`live.ManifestChain`): its own classes
 *  and traits as later extensions see them, in the host manifest's format.
 */
private[scalanative] object LiveDelta {

  def write(
      ext: LiveMetadata.Extension,
      defns: Seq[nir.Defn],
      bundle: java.nio.file.Path
  )(implicit meta: Metadata): Unit =
    ext.config.delta.foreach { path =>
      build(ext, defns, bundle).write(path)
    }

  def build(
      ext: LiveMetadata.Extension,
      defns: Seq[nir.Defn],
      bundle: java.nio.file.Path
  )(implicit meta: Metadata): Manifest = {
    val analysis = meta.analysis
    val open: Set[ScopeInfo] = analysis.openScopes.toSet

    def direct(scope: ScopeInfo, hasSlot: nir.Sig => Boolean) =
      scope.calls.toSeq
        .filter(sig => sig.isVirtual && !hasSlot(sig))
        .sortBy(_.mangle)
        .flatMap { sig =>
          val targets = scope.targets(sig)
          if (targets.size <= 1 && !targets.exists(analysis.isPhantom))
            Some(
              Manifest.DirectEntry(
                sig.mangle,
                targets.headOption.fold("")(_.mangle)
              )
            )
          else None
        }

    val classes = meta.classes.map { cls =>
      val layout = meta.layout(cls)
      val fields = cls.members.collect { case f: Field => f }.toSeq.map { f =>
        val offset = layout.layout.tys(layout.index(f)).offset
        Manifest.FieldEntry(f.name.mangle, offset.toInt, f.ty.mangle)
      }
      val vtable = meta.vtable(cls)
      val flags =
        (if (open.contains(cls)) Manifest.Flags.Open else 0) |
          (if (cls.isModule) Manifest.Flags.Module else 0) |
          (if (cls.isModule && meta.isConstantModule(cls))
             Manifest.Flags.ConstantModule
           else 0) |
          (if (cls.allocated) Manifest.Flags.Allocated else 0) |
          (if (cls.attrs.isAbstract) Manifest.Flags.Abstract else 0)
      Manifest.ClassEntry(
        name = cls.name.id,
        parent = cls.parent.fold("")(_.name.id),
        traits = cls.traits.map(_.name.id),
        flags = flags,
        size = layout.size.toInt,
        fields = fields,
        vtable = vtable.signatures
          .drop(cls.parent.fold(0)(meta.vtable(_).signatures.size))
          .map(_.mangle),
        direct = direct(cls, sig => vtable.index(sig) >= 0)
      )
    }
    val traits = meta.traits.map { trt =>
      Manifest.TraitEntry(
        name = trt.name.id,
        open = open.contains(trt),
        methods = meta.traitMethods(trt).map(_.mangle),
        direct = direct(trt, _ => false)
      )
    }
    Manifest(
      header = ext.chain.host.header.copy(
        hostBinary = bundle.toAbsolutePath().toString
      ),
      classes = classes,
      traits = traits,
      exports = exportedSymbols(ext, defns),
      dynsigs = Nil
    )
  }

  /** The Scala symbols the extension defines, which later extensions bind to:
   *  methods, RTTI, module accessors and constant instances; not the
   *  generator's own tables or the loader's descriptor, nor its copies of
   *  methods of the host and of earlier extensions, which are hidden: a later
   *  extension makes copies of its own.
   */
  private def exportedSymbols(
      ext: LiveMetadata.Extension,
      defns: Seq[nir.Defn]
  )(implicit
      meta: Metadata
  ): Seq[String] =
    defns.iterator
      .collect {
        case defn: nir.Defn.Define => defn.name
        case defn: nir.Defn.Var    => defn.name
        case defn: nir.Defn.Const  => defn.name
      }
      .collect {
        case name @ nir.Global.Member(owner, sig)
            if !sig.isExtern && !owner.id.startsWith("__") &&
              !meta.globalSections.forGlobal(name).exists(_.anonymous) &&
              !ext.manifest.owns(owner) =>
          name.mangle
      }
      .toSeq
      .distinct
      .sorted
}
