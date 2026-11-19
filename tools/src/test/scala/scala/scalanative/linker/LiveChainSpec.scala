package scala.scalanative
package linker

import java.nio.file.Files

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build.LiveExecution
import scala.scalanative.live.{Manifest, ManifestChain}

/** The class name patterns of open and callable sets, and the manifest chain of
 *  chained extensions.
 */
class LiveChainSpec {

  private def header(binary: String) = Manifest.Header(
    targetTriple = "arm64-apple-darwin",
    sizeOfPtr = 8,
    nirCompat = nir.Versions.compat,
    nirRevision = nir.Versions.revision,
    runtimeAbi = Manifest.RuntimeAbiVersion,
    multithreading = true,
    gc = "immix",
    mode = "debug",
    usesLockWords = true,
    yieldPointTraps = false,
    cxxExceptions = false,
    virtualThreads = false,
    hostBinary = binary
  )

  private def cls(name: String, parent: String, flags: Int) =
    Manifest.ClassEntry(
      name = name,
      parent = parent,
      traits = Nil,
      flags = flags,
      size = 16,
      fields = Nil,
      vtable = Seq("D1fiEO"),
      direct = Nil
    )

  @Test def patternsAreGlobsWithExclusions(): Unit = {
    import LiveExecution.Patterns.matches
    val patterns = Seq("*", "-scala.scalanative.windows.*", "-*Windows*")
    assertTrue(matches(patterns, "scala.collection.immutable.List"))
    assertFalse(matches(patterns, "scala.scalanative.windows.HandleApi$"))
    assertFalse(matches(patterns, "java.lang.process.WindowsProcess"))
    assertTrue(matches(Seq("api.*"), "api.Base"))
    assertFalse(matches(Seq("api.*"), "apix.Base"))
    assertTrue(matches(Seq("a*c*e"), "abcde"))
    assertFalse(matches(Seq("a*c*e"), "abcdef"))
    assertTrue(matches(Seq("java.lang.Object"), "java.lang.Object"))
    assertFalse(matches(Seq("-java.lang.Object"), "java.lang.Object"))
  }

  @Test def chainMergesDeltasKeepingTheHostHash(): Unit = {
    val host = Manifest(
      header("/tmp/host"),
      classes = Seq(cls("java.lang.Object", "", Manifest.Flags.Open)),
      traits = Nil,
      exports = Seq("M16java.lang.ObjectD8toStringL16java.lang.StringEO"),
      dynsigs = Nil
    )
    val delta1 = Manifest(
      header("/tmp/line1.bundle"),
      classes =
        Seq(cls("rs$line$1$A", "java.lang.Object", Manifest.Flags.Open)),
      traits = Seq(
        Manifest.TraitEntry("rs$line$1$T", open = true, Seq("D1giEO"), Nil)
      ),
      exports = Seq("M11rs$line$1$AD1fiEO"),
      dynsigs = Nil
    )
    val delta2 = Manifest(
      header("/tmp/line2.bundle"),
      classes = Seq(cls("rs$line$2$B", "rs$line$1$A", Manifest.Flags.Open)),
      traits = Nil,
      exports = Seq("M11rs$line$2$BD1fiEO"),
      dynsigs = Nil
    )
    val dir = Files.createTempDirectory("chain")
    val paths = Seq("host.snm", "delta1.snm", "delta2.snm").map(dir.resolve)
    Seq(host, delta1, delta2).zip(paths).foreach { case (m, p) => m.write(p) }
    val chain = ManifestChain.of(paths.head, paths.tail)
    val merged = chain.merged
    assertEquals(host.hash, chain.hostHash)
    assertEquals(
      Seq("java.lang.Object", "rs$line$1$A", "rs$line$2$B"),
      merged.classes.map(_.name)
    )
    assertEquals(
      Seq("D1fiEO", "D1fiEO", "D1fiEO"),
      merged.allSlots("rs$line$2$B")
    )
    assertTrue(merged.owns(nir.Global.Top("rs$line$1$T")))
    assertTrue(merged.exportSet.contains("M11rs$line$1$AD1fiEO"))
    assertTrue(chain.hostDefines(nir.Global.Top("java.lang.Object")))
    assertFalse(chain.hostDefines(nir.Global.Top("rs$line$1$A")))
    assertEquals(
      Set("rs$line$1$A", "rs$line$1$T", "rs$line$2$B"),
      chain.fromExtensions
    )
    // A chain is read once per set of files
    assertSame(chain, ManifestChain.of(paths.head, paths.tail))
  }
}
