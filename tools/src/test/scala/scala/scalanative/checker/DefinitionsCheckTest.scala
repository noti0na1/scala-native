package scala.scalanative
package checker

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.linker.{Method, ReachabilityAnalysis}

class DefinitionsCheckTest extends LinkerSpec {

  private val source =
    """|object Test {
       |  def main(args: Array[String]): Unit = println(args.length)
       |}
       |""".stripMargin

  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition
  private implicit val scope: nir.ScopeId = nir.ScopeId.TopLevel

  private def local(id: Long, ty: nir.Type = nir.Type.Int) =
    nir.Val.Local(nir.Local(id), ty)
  private def label(id: Long, params: nir.Val.Local*) =
    nir.Inst.Label(nir.Local(id), params)
  private def let(id: Long, op: nir.Op, unwind: nir.Next = nir.Next.None) =
    nir.Inst.Let(nir.Local(id), op, unwind)
  private def add(l: nir.Val, r: nir.Val) =
    nir.Op.Bin(nir.Bin.Iadd, nir.Type.Int, l, r)
  private def to(id: Long, args: nir.Val*) = nir.Next.Label(nir.Local(id), args)
  private def unwindTo(exc: Long, handler: Long) = {
    val e = local(exc, nir.Rt.Throwable)
    nir.Next.Unwind(e, to(handler, e))
  }

  /** The checker's errors on a method `insts` of type `int => int`. */
  private def errors(insts: nir.Inst*): Seq[String] = {
    var result: Seq[String] = null
    link("Test", source) {
      case (_, analysis) =>
        implicit val linked: ReachabilityAnalysis.Result = analysis
        val owner = analysis.infos(nir.Global.Top("Test$"))
        val name = nir.Global.Top("Test$").member(nir.Sig.Method("probe", Nil))
        val ty = nir.Type.Function(Seq(nir.Type.Int), nir.Type.Int)
        val meth = new Method(
          nir.Attrs.None,
          owner,
          name,
          ty,
          insts.toArray,
          nir.Defn.Define.DebugInfo.empty
        )
        val check = new Check
        check.checkMethod(meth)
        result = check.errors.toSeq.map(_.msg)
    }
    result
  }

  private def assertDefined(insts: nir.Inst*): Unit =
    assertEquals(Nil, errors(insts: _*))

  private def assertNotDefined(local: String, insts: nir.Inst*): Unit =
    assertEquals(
      Seq(s"$local is not defined on every path to its use"),
      errors(insts: _*)
    )

  @Test def definedBeforeTheUse(): Unit =
    assertDefined(
      label(0, local(1)),
      let(2, add(local(1), nir.Val.Int(1))),
      nir.Inst.Jump(to(3, local(2))),
      label(3, local(4)),
      nir.Inst.Ret(local(4))
    )

  @Test def usedBeforeItsDefinition(): Unit =
    assertNotDefined(
      "%3",
      label(0, local(1)),
      let(2, add(local(1), local(3))),
      let(3, add(local(1), nir.Val.Int(1))),
      nir.Inst.Ret(local(2))
    )

  @Test def definedOnOneBranchOnly(): Unit =
    assertNotDefined(
      "%3",
      label(0, local(1)),
      let(2, nir.Op.Comp(nir.Comp.Ieq, nir.Type.Int, local(1), nir.Val.Int(0))),
      nir.Inst.If(local(2, nir.Type.Bool), to(10), to(20)),
      label(10),
      let(3, add(local(1), nir.Val.Int(1))),
      nir.Inst.Jump(to(30)),
      label(20),
      nir.Inst.Jump(to(30)),
      label(30),
      nir.Inst.Ret(local(3))
    )

  @Test def handlerUsesWhatIsDefinedBeforeTheThrow(): Unit =
    assertDefined(
      label(0, local(1)),
      let(2, add(local(1), nir.Val.Int(1))),
      let(3, add(local(2), nir.Val.Int(1)), unwindTo(4, 10)),
      nir.Inst.Ret(local(3)),
      label(10, local(11, nir.Rt.Throwable)),
      nir.Inst.Ret(local(2))
    )

  @Test def handlerUsesTheResultOfTheThrowingInstruction(): Unit =
    assertNotDefined(
      "%3",
      label(0, local(1)),
      let(3, add(local(1), nir.Val.Int(1)), unwindTo(4, 10)),
      nir.Inst.Ret(local(3)),
      label(10, local(11, nir.Rt.Throwable)),
      nir.Inst.Ret(local(3))
    )

  @Test def exceptionOutsideItsEdge(): Unit =
    assertNotDefined(
      "%4",
      label(0, local(1)),
      let(3, add(local(1), nir.Val.Int(1)), unwindTo(4, 10)),
      nir.Inst.Ret(local(3)),
      label(10, local(11, nir.Rt.Throwable)),
      let(12, nir.Op.Copy(local(4, nir.Rt.Throwable))),
      nir.Inst.Ret(local(1))
    )

  @Test def unreachableCodeIsNotChecked(): Unit =
    assertDefined(
      label(0, local(1)),
      nir.Inst.Ret(local(1)),
      label(10),
      nir.Inst.Ret(local(99))
    )
}
