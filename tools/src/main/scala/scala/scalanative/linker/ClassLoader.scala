package scala.scalanative
package linker

import scala.collection.mutable

import scala.scalanative.build.Logger
import scalanative.io.VirtualDirectory
import scalanative.util.Scope

sealed abstract class ClassLoader {

  def classesWithEntryPoints: Iterable[nir.Global.Top]

  def definedServicesProviders: Map[nir.Global.Top, Iterable[nir.Global.Top]]

  def load(global: nir.Global.Top): Option[Seq[nir.Defn]]

  /** Every class, trait and module this loader can load. */
  def classes: Iterable[nir.Global.Top]

  /** Whether this loader has a definition of `global`. */
  def contains(global: nir.Global.Top): Boolean = load(global).isDefined

}

object ClassLoader {

  def fromDisk(config: build.Config)(implicit in: Scope): ClassLoader = {
    val classpath = config.classPath.map { path =>
      ClassPathCache
        .lookup(path, config.logger)
        .getOrElse(ClassPath(VirtualDirectory.real(path), config.logger))
    }
    config.compilerConfig.liveExecution match {
      case ext: build.LiveExecution.Extension =>
        new HostView(new FromDisk(classpath), live.ManifestChain.manifest(ext))
      case _ => new FromDisk(classpath)
    }
  }

  /** For an extension link, loads the host's classes as the host link kept
   *  them: without the mixin forwarders it left out (`live.HostForwarders`), so
   *  that the extension resolves those methods to the traits' implementations,
   *  as the host does; and without the methods whose signature needs a class
   *  that does not exist, which the host left out too (`Thread.join` of a
   *  `java.time.Duration`, which a subclass of `Thread` would otherwise
   *  inherit).
   */
  final class HostView(underlying: ClassLoader, manifest: live.Manifest)
      extends ClassLoader {
    def classesWithEntryPoints: Iterable[nir.Global.Top] =
      underlying.classesWithEntryPoints
    def definedServicesProviders
        : Map[nir.Global.Top, Iterable[nir.Global.Top]] =
      underlying.definedServicesProviders
    private val forwarders = new live.HostForwarders.Finder(underlying)
    private val existing = mutable.Map.empty[nir.Global.Top, Boolean]
    private def needsMissingClass(ty: nir.Type): Boolean = ty match {
      case nir.Type.Function(args, ret) =>
        args.exists(needsMissingClass) || needsMissingClass(ret)
      case nir.Type.Array(elem, _)  => needsMissingClass(elem)
      case nir.Type.Ref(name, _, _) =>
        !existing.getOrElseUpdate(name, underlying.contains(name))
      case _ => false
    }
    def load(global: nir.Global.Top): Option[Seq[nir.Defn]] = {
      val defns = underlying.load(global)
      def exported(name: nir.Global) = manifest.exportSet.contains(name.mangle)
      val unexported = defns.exists(_.exists {
        case defn: nir.Defn.Define => !exported(defn.name)
        case _: nir.Defn.Declare   => true
        case _                     => false
      })
      if (!unexported || !manifest.owns(global)) defns
      else {
        val missing = defns.toSeq.flatten.collect {
          case d: nir.Defn.Define
              if !exported(d.name) && needsMissingClass(d.ty) =>
            d.name
          case d: nir.Defn.Declare if needsMissingClass(d.ty) => d.name
        }
        val leftOut = forwarders(global).keySet.filterNot(exported) ++ missing
        if (leftOut.isEmpty) defns
        else defns.map(_.filterNot(defn => leftOut.contains(defn.name)))
      }
    }
    def classes: Iterable[nir.Global.Top] = underlying.classes
    override def contains(global: nir.Global.Top): Boolean =
      underlying.contains(global)
  }

  def fromMemory(defns: Seq[nir.Defn]): ClassLoader =
    new FromMemory(defns)

  final class FromDisk(classpath: Seq[ClassPath]) extends ClassLoader {
    lazy val classesWithEntryPoints: Iterable[nir.Global.Top] = {
      classpath.flatMap(_.classesWithEntryPoints)
    }
    lazy val definedServicesProviders
        : Map[nir.Global.Top, Iterable[nir.Global.Top]] =
      classpath.flatMap(_.definedServicesProviders).toMap

    def load(global: nir.Global.Top): Option[Seq[nir.Defn]] =
      classpath.collectFirst {
        case path if path.contains(global) =>
          path.load(global)
      }.flatten

    def classes: Iterable[nir.Global.Top] =
      classpath.flatMap(_.classes).distinct

    override def contains(global: nir.Global.Top): Boolean =
      classpath.exists(_.contains(global))
  }

  final class FromMemory(defns: Seq[nir.Defn]) extends ClassLoader {

    private val scopes = {
      val out =
        mutable.Map.empty[nir.Global.Top, mutable.UnrolledBuffer[nir.Defn]]
      defns.foreach { defn =>
        val owner = defn.name.top
        val buf =
          out.getOrElseUpdate(owner, mutable.UnrolledBuffer.empty[nir.Defn])
        buf += defn
      }
      out
    }

    lazy val classesWithEntryPoints: Iterable[nir.Global.Top] = {
      scopes.filter {
        case (_, defns) => defns.exists(_.isEntryPoint)
      }.keySet
    }

    def definedServicesProviders
        : Map[nir.Global.Top, Iterable[nir.Global.Top]] =
      Map.empty

    def load(global: nir.Global.Top): Option[Seq[nir.Defn]] =
      scopes.get(global).map(_.toSeq)

    def classes: Iterable[nir.Global.Top] = scopes.keys

  }

  /** Loads the classes `owned` accepts from `primary` and every other class
   *  from `fallback`. An extension link reads the host's classes from the class
   *  path and its own from memory once they are optimized, with its optimized
   *  copies of host methods.
   */
  final class Combined(
      primary: ClassLoader,
      fallback: ClassLoader,
      owned: nir.Global.Top => Boolean
  ) extends ClassLoader {
    lazy val classesWithEntryPoints: Iterable[nir.Global.Top] =
      primary.classesWithEntryPoints.filter(owned) ++
        fallback.classesWithEntryPoints.filterNot(owned)
    def definedServicesProviders
        : Map[nir.Global.Top, Iterable[nir.Global.Top]] =
      fallback.definedServicesProviders
    // An owned class's methods that `fallback` defines (as `define`, not
    // `declare`) replace or join the ones `primary` has. Its declarations of
    // members that `primary` lacks join them too: methods the linker generated
    // in the owner's link, such as ServiceLoader's provider loaders, which no
    // NIR file has
    def load(global: nir.Global.Top): Option[Seq[nir.Defn]] =
      if (owned(global)) primary.load(global).map { defns =>
        val own = defns.iterator.map(_.name).toSet
        val added = fallback
          .load(global)
          .fold(Seq.empty[nir.Defn])(_.filter {
            case _: nir.Defn.Define     => true
            case defn: nir.Defn.Declare => !own.contains(defn.name)
            case _                      => false
          })
        if (added.isEmpty) defns
        else {
          val names = added.iterator.map(_.name).toSet
          defns.filterNot(defn => names.contains(defn.name)) ++ added
        }
      }
      else fallback.load(global)
    def classes: Iterable[nir.Global.Top] =
      primary.classes.filter(owned) ++ fallback.classes.filterNot(owned)
  }

  /** Loads each class from `underlying` and passes its definitions through
   *  `rewrite`, which returns None to hide the class. `live.HostStubs` uses it
   *  for host code that does not link.
   */
  final class Rewriting(
      underlying: ClassLoader,
      rewrite: (nir.Global.Top, Seq[nir.Defn]) => Option[Seq[nir.Defn]]
  ) extends ClassLoader {
    // Not the classes the rewrite leaves out
    def classesWithEntryPoints: Iterable[nir.Global.Top] =
      underlying.classesWithEntryPoints.filter(load(_).isDefined)
    def definedServicesProviders
        : Map[nir.Global.Top, Iterable[nir.Global.Top]] =
      underlying.definedServicesProviders
    def load(global: nir.Global.Top): Option[Seq[nir.Defn]] =
      underlying.load(global).flatMap(rewrite(global, _))
    def classes: Iterable[nir.Global.Top] = underlying.classes
  }

}
