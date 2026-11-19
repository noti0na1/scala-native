package scala.scalanative
package live

import java.nio.file.{Files, Path}

import scala.collection.mutable

import scala.scalanative.build.{BuildException, LiveExecution}

/** Extensions linked against earlier extensions, as a REPL links each line
 *  against the lines before it.
 *
 *  An extension link that is given a delta path writes a *delta*: a manifest in
 *  the host manifest's format that describes the classes and traits the
 *  extension defines (shapes, vtable slots, itable method orders, the calls it
 *  devirtualized) and the symbols it exports. A later extension links against
 *  the host's manifest plus the deltas of the extensions loaded before it, its
 *  *chain*, and its bundle binds to their symbols when it is loaded.
 *
 *  A chained extension's own classes and traits are open: its code dispatches
 *  virtually on them, as later extensions may subclass and implement them.
 *  Their RTTI is complete when the extension is linked, as a host's is, so a
 *  later extension's classes refer to it in their lineages and itables.
 */
private[scalanative] final class ManifestChain(
    val host: Manifest,
    val deltas: Seq[Manifest]
) {

  /** The cache key of this chain, set by `ManifestChain.of`. */
  private[live] var key: Seq[(Path, Long)] = Nil
  private[live] def keyWithout(n: Int): Seq[(Path, Long)] = key.dropRight(n)

  /** The host's manifest with the deltas' classes, traits and exports. Its
   *  lookups extend those of the chain without the last delta, so that the
   *  host's are built once per process, not once per link.
   */
  lazy val merged: Manifest =
    if (deltas.isEmpty) host
    else {
      val previous = ManifestChain.previous(this)
      val m = Manifest(
        header = host.header,
        classes = previous.classes ++ deltas.last.classes,
        traits = previous.traits ++ deltas.last.traits,
        exports = previous.exports ++ deltas.last.exports,
        dynsigs = host.dynsigs
      )
      m.extending = Some(previous)
      m
    }

  /** The hash extensions embed and the loader compares: the host's. */
  def hostHash: String = host.hash

  /** The classes and traits earlier extensions define. */
  lazy val fromExtensions: Set[String] =
    deltas.iterator
      .flatMap(d =>
        d.classes.iterator.map(_.name) ++ d.traits.iterator.map(_.name)
      )
      .toSet

  /** Whether the host itself defines it, not an earlier extension. */
  def hostDefines(name: nir.Global.Top): Boolean = host.owns(name)
}

private[scalanative] object ManifestChain {
  private val cache =
    mutable.Map.empty[Seq[(Path, Long)], ManifestChain]

  /** The chain of an extension link: its host's manifest and its deltas, cached
   *  by path and modification time.
   */
  def apply(ext: LiveExecution.Extension): ManifestChain =
    of(ext.hostManifest, ext.chain)

  def of(hostManifest: Path, deltas: Seq[Path]): ManifestChain = {
    val paths = hostManifest +: deltas
    val key = paths.map { p =>
      val abs = p.toAbsolutePath()
      if (!Files.exists(abs))
        throw new BuildException(s"Manifest not found: $abs")
      abs -> Files.getLastModifiedTime(abs).toMillis()
    }
    cache.synchronized {
      cache.getOrElseUpdate(
        key, {
          // Keep the cache small: a REPL adds one chain per line, each
          // extending the one before, so evict the chains that are at least
          // two deltas shorter than this one
          if (cache.size > 16)
            cache.keys.toList
              .filter(_.size + 2 <= key.size)
              .foreach(cache.remove)
          val host = Manifest.read(hostManifest)
          val ds = deltas.map(Manifest.read)
          ds.zip(deltas).foreach {
            case (d, path) =>
              if (d.header.targetTriple != host.header.targetTriple ||
                  d.header.multithreading != host.header.multithreading)
                throw new BuildException(
                  s"Delta $path was written for another host"
                )
          }
          val chain = new ManifestChain(host, ds)
          chain.key = key
          chain
        }
      )
    }
  }

  /** The merged manifest of `chain` without its last delta. */
  private def previous(chain: ManifestChain): Manifest =
    if (chain.deltas.size <= 1) chain.host
    else {
      val key = chain.keyWithout(1)
      cache.synchronized(cache.get(key)) match {
        case Some(c) => c.merged
        case None    => new ManifestChain(chain.host, chain.deltas.init).merged
      }
    }

  /** The manifest an extension link reads: the host's, with its chain. */
  def manifest(ext: LiveExecution.Extension): Manifest =
    if (ext.chain.isEmpty) Manifest.read(ext.hostManifest)
    else apply(ext).merged
}
