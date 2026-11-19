package scala.scalanative
package codegen

import java.nio.file.Files

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build.LiveExecution
import scala.scalanative.live.Manifest

/** Code generation of live-execution hosts: the export list, and the dispatch
 *  tables of classes outside the boundary, which keep no implementation of a
 *  method that only made-up calls ask for.
 */
class LiveHostCodeGenTest extends CodeGenSpec {

  private val source = Map(
    "Test.scala" ->
      """
      trait Api { def a(): Int; def b(): Int }
      final class One extends Api { def a(): Int = 1; def b(): Int = 10 }
      final class Two extends Api { def a(): Int = 2; def b(): Int = 20 }
      abstract class Base { def a(): Int; def b(): Int }
      final class Three extends Base { def a(): Int = 3; def b(): Int = 30 }
      final class Four extends Base { def a(): Int = 4; def b(): Int = 40 }
      object Test {
        def main(args: Array[String]): Unit = {
          val x: Api = if (args.length > 0) new One else new Two
          val y: Base = if (args.length > 1) new Three else new Four
          println(x.a() + y.a())
        }
      }
      """
  )

  // One and Four are outside the boundary: they keep no b()
  private def host(c: build.NativeConfig) =
    c.withLiveExecution(
      LiveExecution.Host(
        open = Nil,
        callable = Seq("Api", "Two", "Base", "Three")
      )
    )

  @Test def elfExportListLeavesPatternsUnquoted(): Unit =
    codegen(
      "Test",
      source,
      c => host(c).withTargetTriple("aarch64-unknown-linux-gnu")
    ) { (config, _, _) =>
      val list = new String(
        Files.readAllBytes(LiveBoundary.exportListPath(config)),
        "UTF-8"
      )
      val lines = list.linesIterator.toSeq
      // GNU ld matches a quoted name literally
      assertTrue(list, lines.contains("scalanative_*;"))
      assertFalse(list, lines.contains("\"scalanative_*\";"))
      assertTrue(list, lines.exists(_.startsWith("\"_S")))
    }

  private val b: nir.Sig = nir.Sig.Method("b", Seq(nir.Type.Int))

  private def isStub(value: nir.Val): Boolean = value match {
    case nir.Val.Global(nir.Global.Member(owner, _), _) =>
      owner == LiveMetadata.HostStubOwner
    case _ => false
  }

  @Test def entriesWithoutImplementationAreStubs(): Unit =
    codegen("Test", source, host) { (config, analysis, outfiles) =>
      def cls(name: String) =
        analysis.infos(nir.Global.Top(name)).asInstanceOf[linker.Class]
      val api = analysis.infos(nir.Global.Top("Api")).asInstanceOf[linker.Trait]
      assertEquals(None, cls("One").resolve(b))
      assertEquals(None, cls("Four").resolve(b))
      assertTrue(
        "Api has a made-up call of b()",
        api.calls.contains(b)
      )

      val meta = new Metadata(analysis, config, Nil)(PlatformInfo(config))
      assertTrue(meta.live.lacksImplementations(api, b))
      assertTrue(meta.live.lacksImplementations(cls("Base"), b))

      def itableEntry(name: String): nir.Val = {
        val index = meta.traitMethods(api).indexOf(b)
        val slots = meta.itable(cls(name)).value match {
          case nir.Val.Const(nir.Val.ArrayValue(_, slots)) => slots
          case _                                           => Nil
        }
        slots.collectFirst {
          case nir.Val.StructValue(
                Seq(trt, nir.Val.Const(nir.Val.ArrayValue(_, methods)))
              ) if trt == meta.rtti(api).const =>
            methods(index)
        }.get
      }
      assertTrue(isStub(itableEntry("One")))
      assertEquals(
        nir.Val.Global(nir.Global.Top("Two").member(b), nir.Type.Ptr),
        itableEntry("Two")
      )

      // Base dispatches b() through its vtable: Four's entry is a stub
      val slot = meta.vtable(cls("Base")).index(b)
      assertTrue("Base has a vtable slot for b()", slot >= 0)
      assertTrue(isStub(meta.vtable(cls("Four")).at(slot)))
      assertEquals(
        nir.Val.Global(nir.Global.Top("Three").member(b), nir.Type.Ptr),
        meta.vtable(cls("Three")).at(slot)
      )

      // The stub throws, naming the method
      val stubs = meta.live.asInstanceOf[LiveMetadata.Host].stubDefns
      assertEquals(1, stubs.size)
      assertTrue(stubs.head.insts.exists {
        case nir.Inst
              .Let(_, nir.Op.Call(_, f, Seq(_, nir.Val.String(msg))), _) =>
          f == Lower.throwNoSuchMethodVal && msg.startsWith("b() is not linked")
        case _ => false
      })

      // Extensions are told to dispatch, not to call the boundary class's b()
      val manifest = Manifest.read(LiveBoundary.manifestPath(config))
      assertFalse(manifest.traitByName("Api").directBySig.contains(b.mangle))
      assertFalse(manifest.classByName("Base").directBySig.contains(b.mangle))
      assertTrue(manifest.allSlots("Base").contains(b.mangle))
      assertFalse(
        "stubs are not exported",
        manifest.exports.exists(_.contains(LiveMetadata.HostStubOwner.id))
      )
      val ir =
        outfiles.iterator.map(p => new String(Files.readAllBytes(p))).mkString
      assertTrue(
        "the stub is emitted",
        ir.contains("define") && ir.contains(LiveMetadata.HostStubOwner.id)
      )
    }
}
