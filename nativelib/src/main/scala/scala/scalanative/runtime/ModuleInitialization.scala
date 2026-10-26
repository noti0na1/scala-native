package scala.scalanative
package runtime

import scala.scalanative.unsafe._

// Not in the runtime package object: its @extern annotation, from unsafe's
// package object, causes a cyclic reference on Scala 2.12
@extern private[runtime] object ModuleInitialization {
  @name("scalanative_isModuleInitializationContext")
  def isContext(context: RawPtr): Boolean = extern

  @name("scalanative_moduleInitializationInstanceForCurrentThread")
  def instanceForCurrentThread(context: RawPtr): AnyRef = extern

  @name("scalanative_completeModuleInitializationForCurrentThread")
  def completeForCurrentThread(): Unit = extern
}
