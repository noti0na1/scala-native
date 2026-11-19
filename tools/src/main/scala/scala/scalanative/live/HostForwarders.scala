package scala.scalanative
package live

import scala.collection.mutable

import scala.scalanative.linker.ClassLoader

/** Mixin forwarders of callable classes, which a host link leaves out: a
 *  class's method whose body only calls the same method of a trait it mixes in.
 *  Classes of the standard library often have dozens, and keeping them callable
 *  would make each one a function of its own. Without them, reachability
 *  resolves the class's method to the trait's (a class responds with a trait's
 *  method when it defines none), so dispatch tables and calls name the trait's
 *  method, which does the same.
 *
 *  A forwarder stays when it decides something: when a superclass defines the
 *  method too, or when more than one of the class's traits does. A host link
 *  adds back a left-out forwarder that code calls by name, as a subclass's
 *  `super` call does (`HostStubs`); an extension link leaves out the forwarders
 *  its host does not export (`ClassLoader.HostView`).
 */
private[scalanative] object HostForwarders {

  /** The forwarders of `classes` that can be left out, by name. */
  def find(
      loader: ClassLoader,
      classes: Seq[nir.Global.Top]
  ): Map[nir.Global, nir.Defn.Define] = {
    val finder = new Finder(loader)
    classes.flatMap(finder(_)).toMap
  }

  /** Finds the forwarders that can be left out, class by class, reading each
   *  class and trait once.
   */
  final class Finder(loader: ClassLoader) {
    private val defnsCache = mutable.Map.empty[nir.Global.Top, Seq[nir.Defn]]

    private def defns(top: nir.Global.Top): Seq[nir.Defn] =
      defnsCache.getOrElseUpdate(top, loader.load(top).getOrElse(Nil))

    private def scope(top: nir.Global.Top): Option[nir.Defn] =
      defns(top).collectFirst {
        case d: nir.Defn.Class  => d
        case d: nir.Defn.Module => d
        case d: nir.Defn.Trait  => d
      }

    private def isTrait(top: nir.Global.Top) =
      scope(top).exists(_.isInstanceOf[nir.Defn.Trait])

    private def defines(top: nir.Global.Top, sig: nir.Sig): Boolean =
      defns(top).exists {
        case d: nir.Defn.Define => d.name.sig == sig
        case _                  => false
      }

    private def parentAndTraits(
        top: nir.Global.Top
    ): (Option[nir.Global.Top], Seq[nir.Global.Top]) =
      scope(top) match {
        case Some(nir.Defn.Class(_, _, parent, traits))  => (parent, traits)
        case Some(nir.Defn.Module(_, _, parent, traits)) => (parent, traits)
        case Some(nir.Defn.Trait(_, _, traits))          => (None, traits)
        case _                                           => (None, Nil)
      }

    /** The forwarders of class `cls` that can be left out. */
    def apply(cls: nir.Global.Top): Map[nir.Global, nir.Defn.Define] =
      if (!scope(cls).exists(!_.isInstanceOf[nir.Defn.Trait])) Map.empty
      else {
        // Superclasses, and every trait of the class and of its superclasses
        val ancestors = mutable.ArrayBuffer.empty[nir.Global.Top]
        val traits = mutable.LinkedHashSet.empty[nir.Global.Top]
        def addTrait(t: nir.Global.Top): Unit =
          if (traits.add(t)) parentAndTraits(t)._2.foreach(addTrait)
        var current: Option[nir.Global.Top] = Some(cls)
        while (current.isDefined) {
          val (parent, ts) = parentAndTraits(current.get)
          ts.foreach(addTrait)
          parent.foreach(ancestors += _)
          current = parent
        }
        defns(cls).iterator
          .collect {
            case defn: nir.Defn.Define =>
              forwarderTarget(defn).collect {
                case target
                    if isTrait(target.top) && traits.contains(target.top) &&
                      traits.count(defines(_, defn.name.sig)) == 1 &&
                      !ancestors.exists(defines(_, defn.name.sig)) =>
                  (defn.name: nir.Global) -> defn
              }
          }
          .flatten
          .toMap
      }
  }

  /** The method a forwarder calls: the same signature, the same arguments. */
  private def forwarderTarget(defn: nir.Defn.Define): Option[nir.Global] =
    defn.insts match {
      case Seq(
            nir.Inst.Label(_, params),
            nir.Inst.Let(
              result,
              nir.Op.Call(
                _,
                nir.Val.Global(target @ nir.Global.Member(_, sig), _),
                args
              ),
              nir.Next.None
            ),
            nir.Inst.Ret(value)
          )
          if sig == defn.name.sig && args == params &&
            (value match {
              case nir.Val.Local(id, _) => id == result
              case nir.Val.Unit         => true
              case _                    => false
            }) =>
        Some(target)
      case _ => None
    }
}
