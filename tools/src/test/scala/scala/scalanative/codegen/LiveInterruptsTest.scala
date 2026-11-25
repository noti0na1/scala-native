package scala.scalanative
package codegen

import org.junit.Assert._
import org.junit.Test

/** Where `LiveInterrupts` puts its polls: at the entry of a method and before
 *  each jump back to an earlier block, not before forward jumps.
 */
class LiveInterruptsTest {
  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
  private implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel

  private val module = nir.Global.Top("Handler$")
  private val handler =
    module.member(nir.Sig.Method("interrupted", Seq(nir.Type.Unit)))
  private val handlerTy =
    nir.Type.Function(Seq(nir.Type.Ref(module)), nir.Type.Unit)

  private def isPoll(inst: nir.Inst): Boolean = inst match {
    case nir.Inst.Let(
          _,
          nir.Op.Load(_, nir.Val.Global(LiveInterrupts.FlagName, _), _),
          _
        ) =>
      true
    case _ => false
  }

  private def callsHandler(inst: nir.Inst): Boolean = inst match {
    case nir.Inst.Let(_, nir.Op.Call(_, nir.Val.Global(`handler`, _), _), _) =>
      true
    case _ => false
  }

  @Test def pollsAtEntryAndBeforeBackEdges(): Unit = {
    implicit val fresh: nir.Fresh = nir.Fresh()
    // entry: jump loop; loop: if c then body else exit;
    // body: jump loop; exit: ret
    val entry, loop, body, exit = fresh()
    val c = nir.Val.Local(fresh(), nir.Type.Bool)
    val insts = Seq(
      nir.Inst.Label(entry, Nil),
      nir.Inst.Jump(nir.Next(loop)),
      nir.Inst.Label(loop, Nil),
      nir.Inst.Let(
        c.id,
        nir.Op.Comp(nir.Comp.Ieq, nir.Type.Int, nir.Val.Int(0), nir.Val.Int(0)),
        nir.Next.None
      ),
      nir.Inst.If(c, nir.Next(body), nir.Next(exit)),
      nir.Inst.Label(body, Nil),
      nir.Inst.Jump(nir.Next(loop)),
      nir.Inst.Label(exit, Nil),
      nir.Inst.Ret(nir.Val.Unit)
    )
    val defn = nir.Defn.Define(
      nir.Attrs.None,
      nir.Global.Top("Main$").member(nir.Sig.Method("run", Seq(nir.Type.Unit))),
      nir.Type.Function(Nil, nir.Type.Unit),
      insts
    )
    val out = LiveInterrupts.withPolls(defn, module, handler, handlerTy).insts

    assertEquals(2, out.count(isPoll))
    assertEquals(2, out.count(callsHandler))
    // The first poll comes right after the entry label
    assertTrue(isPoll(out(1)))
    // The forward jump to `loop` is not polled: it follows the first poll's
    // resume label
    val forward = out.indexWhere {
      case nir.Inst.Jump(nir.Next.Label(`loop`, _)) => true
      case _                                        => false
    }
    assertTrue(out.take(forward).count(isPoll) == 1)
    // The back-edge in `body` is: a poll sits between its label and the jump
    val bodyAt = out.indexWhere {
      case nir.Inst.Label(`body`, _) => true
      case _                         => false
    }
    val backEdge = out.lastIndexWhere {
      case nir.Inst.Jump(nir.Next.Label(`loop`, _)) => true
      case _                                        => false
    }
    assertTrue(bodyAt < backEdge)
    assertEquals(1, out.slice(bodyAt, backEdge).count(isPoll))
    // The method still ends the same way
    assertEquals(nir.Inst.Ret(nir.Val.Unit), out.last)
  }
}
