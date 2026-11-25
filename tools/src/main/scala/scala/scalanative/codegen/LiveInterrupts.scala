package scala.scalanative
package codegen

import scala.collection.mutable

import scala.scalanative.build.{BuildException, LiveExecution}

/** Interrupt polls in extension code: at the entry of each of the extension's
 *  own methods and before each jump back to an earlier block (a loop), the code
 *  reads the host's interrupt flag (`scalanative_ext_interrupt_flag`,
 *  ext_loader.c) and, when it is set, calls the interrupt handler the extension
 *  is linked with, a host method that throws. A busy loop such as
 *  `while true do ()` stops at its next iteration.
 *
 *  The flag is read with a monotonic load, so that the optimizer cannot hoist
 *  it out of a loop; the handler call is out of line.
 */
private[scalanative] object LiveInterrupts {
  val FlagName: nir.Global.Member =
    nir.Global.Member(
      nir.Global.Top("__"),
      nir.Sig.Extern("scalanative_ext_interrupt_flag")
    )

  /** The handler method named by `<module>.<method>`. */
  def handler(ext: LiveExecution.Extension): Option[nir.Global.Member] =
    ext.interruptHandler.map { spec =>
      val dot = spec.lastIndexOf('.')
      if (dot <= 0 || dot == spec.length - 1)
        throw new BuildException(
          s"Interrupt handler $spec: expected <module>.<method>"
        )
      nir.Global
        .Top(spec.substring(0, dot))
        .member(nir.Sig.Method(spec.substring(dot + 1), Seq(nir.Type.Unit)))
    }

  /** Adds the polls to the methods the extension defines. */
  def apply(ext: LiveMetadata.Extension, defns: Seq[nir.Defn]): Seq[nir.Defn] =
    handler(ext.config).fold(defns) { handler =>
      val module = handler.owner.asInstanceOf[nir.Global.Top]
      val handlerTy =
        nir.Type.Function(Seq(nir.Type.Ref(module)), nir.Type.Unit)
      def isOwnCode(defn: nir.Defn.Define): Boolean = {
        val sig = defn.name.sig
        val owner = defn.name.top
        (sig.isMethod || sig.isCtor) && !owner.id.startsWith("__") &&
          !ext.manifest.owns(owner)
      }
      val polled = defns.map {
        case defn: nir.Defn.Define if isOwnCode(defn) && defn.insts.nonEmpty =>
          withPolls(defn, module, handler, handlerTy)
        case defn => defn
      }
      polled :+ nir.Defn.Var(
        nir.Attrs.None.withIsExtern(true),
        FlagName,
        nir.Type.Int,
        nir.Val.Zero(nir.Type.Int)
      )(nir.SourcePosition.NoPosition)
    }

  private[codegen] def withPolls(
      defn: nir.Defn.Define,
      module: nir.Global.Top,
      handler: nir.Global.Member,
      handlerTy: nir.Type.Function
  ): nir.Defn.Define = {
    implicit val fresh: nir.Fresh = nir.Fresh(defn.insts)
    implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
    val positions = mutable.Map.empty[nir.Local, Int]
    defn.insts.zipWithIndex.foreach {
      case (nir.Inst.Label(id, _), i) => positions(id) = i
      case _                          => ()
    }
    val out = mutable.UnrolledBuffer.empty[nir.Inst]
    def poll()(implicit pos: nir.SourcePosition): Unit = {
      val flag = nir.Val.Local(fresh(), nir.Type.Int)
      out += nir.Inst.Let(
        flag.id,
        nir.Op.Load(
          nir.Type.Int,
          nir.Val.Global(FlagName, nir.Type.Ptr),
          Some(nir.MemoryOrder.Monotonic)
        ),
        nir.Next.None
      )
      val set = nir.Val.Local(fresh(), nir.Type.Bool)
      out += nir.Inst.Let(
        set.id,
        nir.Op.Comp(nir.Comp.Ine, nir.Type.Int, flag, nir.Val.Int(0)),
        nir.Next.None
      )
      val interrupted, resume = fresh()
      out += nir.Inst.If(set, nir.Next(interrupted), nir.Next(resume))
      out += nir.Inst.Label(interrupted, Nil)
      val instance = nir.Val.Local(fresh(), nir.Type.Ref(module))
      out += nir.Inst.Let(instance.id, nir.Op.Module(module), nir.Next.None)
      out += nir.Inst.Let(
        nir.Op.Call(
          handlerTy,
          nir.Val.Global(handler, nir.Type.Ptr),
          Seq(instance)
        ),
        nir.Next.None
      )
      out += nir.Inst.Jump(nir.Next(resume))
      out += nir.Inst.Label(resume, Nil)
    }
    var current = 0
    var entry = true
    defn.insts.foreach {
      case inst: nir.Inst.Label =>
        current = positions(inst.id)
        out += inst
        if (entry) {
          entry = false
          poll()(inst.pos)
        }
      case inst @ nir.Inst.Jump(nir.Next.Label(target, _))
          if positions.get(target).exists(_ <= current) =>
        poll()(inst.pos)
        out += inst
      case inst => out += inst
    }
    defn.copy(insts = out.toSeq)(defn.pos)
  }
}
