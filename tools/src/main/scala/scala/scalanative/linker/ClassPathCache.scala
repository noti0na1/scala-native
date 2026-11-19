package scala.scalanative
package linker

import java.nio.file.{Files, Path}

import scala.collection.mutable

import scala.scalanative.build.Logger
import scala.scalanative.io.VirtualDirectory
import scala.scalanative.util.{Scope, defer}

/** Class path entries kept open between links by a long-lived extension linker
 *  (`live.ExtensionLinker`, which `snc-host` links the REPL's lines and macro
 *  code with): a jar is listed once, and the NIR read from it stays
 *  deserialized. Directories are not kept, since their contents change between
 *  links. Off unless `enable` was called.
 */
private[scalanative] object ClassPathCache {
  private final class Entry(
      val mtime: Long,
      val size: Long,
      val classPath: ClassPath,
      val scope: Scope
  )

  @volatile private var enabled = false
  private val entries = mutable.Map.empty[Path, Entry]

  /** Keeps jars open from now on, until `scope` closes. */
  def enable(scope: Scope): Unit = {
    defer {
      enabled = false
      entries.synchronized {
        entries.values.foreach(_.scope.close())
        entries.clear()
      }
    }(scope)
    enabled = true
  }

  /** A kept class path entry for `path`, if the cache is on and it is a jar. */
  def lookup(path: Path, logger: Logger): Option[ClassPath] =
    if (!enabled) None
    else {
      val abs = path.toAbsolutePath()
      if (!Files.isRegularFile(abs)) None
      else {
        val mtime = Files.getLastModifiedTime(abs).toMillis()
        val size = Files.size(abs)
        entries.synchronized {
          entries.get(abs) match {
            case Some(e) if e.mtime == mtime && e.size == size =>
              Some(e.classPath)
            case _ =>
              // New or changed. Links run one at a time, so no link reads the
              // entry of the jar's old contents any more; it is closed before
              // the jar is opened again, so that the new entry does not share
              // its file system.
              entries.remove(abs).foreach(_.scope.close())
              val scope = Scope.unsafe()
              val cp =
                try ClassPath(VirtualDirectory.real(abs)(scope), logger)
                catch {
                  case e: Throwable =>
                    scope.close()
                    throw e
                }
              entries(abs) = new Entry(mtime, size, cp, scope)
              Some(cp)
          }
        }
      }
    }
}
