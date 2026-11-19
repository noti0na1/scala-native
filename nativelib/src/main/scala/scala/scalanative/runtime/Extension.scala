package scala.scalanative.runtime

import scala.scalanative.unsafe._

/** An extension: code linked separately against this program's manifest
 *  (`snlink --extension`) and loaded into it with `dlopen`, sharing its
 *  runtime. The program must be linked as a live-execution host, with the
 *  classes and traits the extension subclasses or implements declared open.
 *
 *  Extensions are never unloaded: heap objects may point to their classes and
 *  constants.
 */
final class Extension private (handle: Ptr[Byte], val path: String) {

  /** The instance of a module the extension defines, by its NIR name, such as
   *  `ext.Entry$`. It is initialized on first access, as any module.
   */
  def module(name: String): AnyRef = Zone.acquire { implicit z =>
    val cname = toCString(name)
    val instance =
      ExtensionLoader.scalanative_ext_module_instance(handle, cname)
    if (instance != null) Intrinsics.castRawPtrToObject(toRawPtr(instance))
    else {
      val accessor =
        ExtensionLoader.scalanative_ext_module_accessor(handle, cname)
      if (accessor == null)
        throw new NoSuchElementException(
          s"Extension $path defines no module $name"
        )
      val load = CFuncPtr.fromPtr[CFuncPtr0[Ptr[Byte]]](accessor)
      val module = load()
      Intrinsics.castRawPtrToObject(toRawPtr(module))
    }
  }

  /** The number of classes and modules the extension defines. */
  def classCount: Int = ExtensionLoader.scalanative_ext_class_count(handle)
}

object Extension {

  // States of an extension's static initializers, which ext_loader.c keeps
  // for each loaded image; `initialize` reads and sets them under the lock of
  // this object
  private final val InitNotRun = 0
  private final val InitRunning = 1
  private final val InitDone = 2
  private final val InitFailed = 3

  /** Loads an extension, registering its modules with the collector, then runs
   *  its static initializers. Loading the same file again neither loads nor
   *  initializes it a second time; a load while another thread runs its
   *  initializers waits for them to finish.
   *
   *  @throws ExtensionLoadError
   *    if the file is not an extension, was linked for another host, or defines
   *    a class another extension defines already, or if its static initializers
   *    failed in an earlier load
   */
  def load(path: String): Extension = load(path, global = false)

  /** Loads an extension as `load(path)` does; with `global`, extensions loaded
   *  after it can bind to its symbols (chained extensions, linked with its
   *  delta).
   */
  def load(path: String, global: Boolean): Extension = {
    val handle = Zone.acquire { implicit z =>
      ExtensionLoader.scalanative_ext_load_with(
        toCString(path),
        if (global) 1 else 0
      )
    }
    if (handle == null)
      throw new ExtensionLoadError(
        fromCString(ExtensionLoader.scalanative_ext_error())
      )
    initialize(handle, path)
    new Extension(handle, path)
  }

  /** Runs the static initializers of an extension the first time it is loaded,
   *  one extension at a time. A load from the initializers themselves, on the
   *  same thread, finds them running and returns.
   */
  private def initialize(handle: Ptr[Byte], path: String): Unit =
    synchronized {
      ExtensionLoader.scalanative_ext_init_state(handle) match {
        case InitNotRun =>
          ExtensionLoader.scalanative_ext_set_init_state(handle, InitRunning)
          val init = ExtensionLoader.scalanative_ext_init_function(handle)
          var done = false
          try {
            if (init != null) {
              val run = CFuncPtr.fromPtr[CFuncPtr0[Unit]](init)
              run()
            }
            done = true
          } finally {
            ExtensionLoader.scalanative_ext_set_init_state(
              handle,
              if (done) InitDone else InitFailed
            )
          }
        case InitFailed =>
          throw new ExtensionLoadError(
            s"$path: its static initializers failed when it was first loaded"
          )
        case _ => ()
      }
    }
}

/** An extension that could not be loaded. */
final class ExtensionLoadError(message: String) extends Error(message)

@extern
@define("__SCALANATIVE_LIVE_EXECUTION")
private[runtime] object ExtensionLoader {
  // dlopen reads and relocates the whole image but touches no heap object: a
  // collection need not wait for it
  @blocking def scalanative_ext_load(path: CString): Ptr[Byte] = extern
  @blocking def scalanative_ext_load_with(
      path: CString,
      global: CInt
  ): Ptr[Byte] = extern
  def scalanative_ext_error(): CString = extern
  def scalanative_ext_module_accessor(
      ext: Ptr[Byte],
      name: CString
  ): Ptr[Byte] = extern
  def scalanative_ext_module_instance(
      ext: Ptr[Byte],
      name: CString
  ): Ptr[Byte] = extern
  def scalanative_ext_init_function(ext: Ptr[Byte]): Ptr[Byte] = extern
  def scalanative_ext_init_state(ext: Ptr[Byte]): CInt = extern
  def scalanative_ext_set_init_state(ext: Ptr[Byte], state: CInt): Unit =
    extern
  def scalanative_ext_class_count(ext: Ptr[Byte]): CInt = extern
}
