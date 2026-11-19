package scala.scalanative
package build

import java.nio.file.Path

/** Live execution: code linked separately (an extension) and loaded with
 *  `dlopen` into a running program (the host), sharing its runtime.
 *
 *  A host stays closed-world except for the types it declares open. A host link
 *  writes a manifest (`live.Manifest`) of the host's classes, traits and
 *  exported symbols; an extension link reads it and uses the host's layouts and
 *  dispatch for the host's types.
 */
sealed abstract class LiveExecution {
  def isHost: Boolean = this.isInstanceOf[LiveExecution.Host]
  def isExtension: Boolean = this.isInstanceOf[LiveExecution.Extension]
  def isDisabled: Boolean = this eq LiveExecution.Disabled
}

object LiveExecution {

  /** An ordinary closed-world link. */
  case object Disabled extends LiveExecution

  /** A host link.
   *
   *  Patterns name classes and traits by their NIR name (`api.Plugin`,
   *  `api.Registry$` for a module). A pattern ending in `*` matches every name
   *  with that prefix; `*` alone matches everything.
   *
   *  @param open
   *    classes and traits extensions may subclass or implement
   *  @param callable
   *    classes whose methods are all kept reachable, out of line and exported,
   *    whether the host calls them or not
   *  @param stubUnlinkable
   *    methods whose code refers to symbols that do not link become stubs that
   *    throw, instead of whole callable classes being dropped from the boundary
   *    (see `live.HostStubs`)
   */
  final case class Host(
      open: Seq[String],
      callable: Seq[String] = Nil,
      stubUnlinkable: Boolean = false
  ) extends LiveExecution {
    def isOpen(name: String): Boolean = Patterns.matches(open, name)
    def isCallable(name: String): Boolean = Patterns.matches(callable, name)
  }

  /** An extension link against the manifest a host link wrote.
   *
   *  @param hostManifest
   *    the host's manifest
   *  @param roots
   *    class path entries whose classes make up the extension: every class in
   *    them and all of its methods are linked; the rest of the class path is
   *    only looked up
   *  @param hostBinary
   *    the host executable the extension's bundle is linked against; the path
   *    in the manifest if absent
   *  @param chain
   *    deltas of the extensions this one is linked against, in the order they
   *    are loaded (`live.ManifestChain`)
   *  @param delta
   *    where to write this extension's delta, so that later extensions can link
   *    against it; its own classes and traits are then open
   *  @param stubUnlinkable
   *    methods whose code refers to symbols that do not link become stubs that
   *    throw, instead of the link failing (see `live.HostStubs.linkExtension`)
   */
  final case class Extension(
      hostManifest: Path,
      roots: Seq[Path],
      hostBinary: Option[Path] = None,
      chain: Seq[Path] = Nil,
      delta: Option[Path] = None,
      stubUnlinkable: Boolean = false
  ) extends LiveExecution {

    /** Whether later extensions may link against this one. */
    def isChained: Boolean = delta.isDefined
  }

  private[scalanative] object Patterns {

    /** Whether a pattern matches `name` and no pattern starting with `-` (an
     *  exclusion, such as `-scala.scalanative.*`) does. In a pattern, `*`
     *  matches any characters.
     */
    def matches(patterns: Seq[String], name: String): Boolean = {
      val (excluded, included) = patterns.partition(_.startsWith("-"))
      included.exists(glob(_, name)) &&
        !excluded.exists(p => glob(p.drop(1), name))
    }

    private def glob(pattern: String, name: String): Boolean =
      if (pattern == "*") true
      else if (!pattern.contains('*')) pattern == name
      else {
        val parts = pattern.split("\\*", -1)
        var pos = 0
        var ok = name.startsWith(parts.head)
        if (ok) pos = parts.head.length
        var i = 1
        while (ok && i < parts.length - 1) {
          val at = name.indexOf(parts(i), pos)
          if (at < 0) ok = false else pos = at + parts(i).length
          i += 1
        }
        ok && name.length - pos >= parts.last.length &&
          name.endsWith(parts.last)
      }
  }
}
