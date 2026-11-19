package scala.scalanative
package linker

import scala.collection.mutable

import scala.scalanative.build.LiveExecution

/** Reachability for live execution: the open world inside the closed-world
 *  analysis.
 *
 *  Host links: each open class gets a phantom subclass and each open trait a
 *  phantom implementor. A phantom is allocated and responds to every
 *  overridable signature of its type with a global that does not exist, so
 *  target sets of open types always hold one member no host code knows about.
 *  Phantoms are never put in `infos` and none of their members is ever reached.
 *  Every overridable signature of an open type counts as called. The methods an
 *  extension subclass would inherit, the constructors of open classes and all
 *  fields of open classes are kept.
 *
 *  Extension links: classes, traits and modules the host manifest lists are the
 *  host's. Their definitions are read for their shape. A reached host method
 *  that the host exports becomes a declaration; one it does not export, which
 *  the host's optimizer inlined everywhere or which the host never calls, is
 *  copied into the extension (`ExtensionContext.copiesMethod`).
 */
private[linker] trait LiveReach { self: Reach =>
  import LiveReach._

  protected lazy val liveHost: Option[LiveExecution.Host] =
    config.compilerConfig.liveExecution match {
      case host: LiveExecution.Host => Some(host)
      case _                        => None
    }

  protected lazy val liveExtension: Option[ExtensionContext] =
    config.compilerConfig.liveExecution match {
      case ext: LiveExecution.Extension =>
        Some(new ExtensionContext(ext, live.ManifestChain.manifest(ext)))
      case _ => None
    }

  val phantomTops = mutable.Set.empty[nir.Global.Top]
  val phantoms = mutable.UnrolledBuffer.empty[Class]
  val openScopes = mutable.UnrolledBuffer.empty[ScopeInfo]
  private val pendingOpen = mutable.UnrolledBuffer.empty[ScopeInfo]
  private val pendingCallable = mutable.UnrolledBuffer.empty[ScopeInfo]

  /** Methods an extension may call or inherit: entries for the optimizer and
   *  the second reachability pass, and exported.
   */
  val boundaryRoots = mutable.LinkedHashSet.empty[nir.Global.Member]

  /** Extension links: host classes the extension allocates although the host
   *  never does, with the methods that allocate them.
   */
  val badAllocations =
    mutable.LinkedHashMap.empty[nir.Global.Top, mutable.LinkedHashSet[
      nir.Global
    ]]

  /** Extension links: extension classes that extend or implement a host type
   *  the host does not declare open, with that type.
   */
  val closedParents =
    mutable.LinkedHashMap.empty[nir.Global.Top, nir.Global.Top]

  /** Extension links: fails on uses of the host the boundary does not allow. An
   *  extension that stubs what does not link (`stubUnlinkable`) reports them as
   *  unreachable instead (`live.HostStubs.linkExtension`): its classes that
   *  extend a closed host type with the class as the referrer, so that the
   *  class is left out and the code that needs it becomes stubs, and the
   *  allocations of host classes the host never allocates with the method that
   *  makes them as the referrer, so that the method becomes a stub.
   */
  def checkBoundary(): Unit = {
    if (liveExtension.exists(_.config.stubUnlinkable)) {
      badAllocations.foreach {
        case (cls, referrers) =>
          referrers.foreach {
            case referrer: nir.Global.Member =>
              self.unreachable.getOrElseUpdate(
                referrer,
                Reach.UnreachableSymbol(
                  name = cls,
                  symbol = self.parseSymbol(cls),
                  backtrace = List(
                    Reach.BackTraceElement(
                      referrer,
                      self.parseSymbol(referrer),
                      "",
                      0
                    )
                  )
                )
              )
            case _ => ()
          }
      }
      badAllocations.keys.toList.foreach { cls =>
        val left =
          badAllocations(cls).filterNot(_.isInstanceOf[nir.Global.Member])
        if (left.isEmpty) badAllocations -= cls
        else badAllocations(cls) = left
      }
      closedParents.foreach {
        case (cls, parent) =>
          self.unreachable.getOrElseUpdate(
            cls,
            Reach.UnreachableSymbol(
              name = parent,
              symbol = self.parseSymbol(parent),
              backtrace = List(
                Reach.BackTraceElement(cls, self.parseSymbol(cls), "", 0)
              )
            )
          )
      }
      closedParents.clear()
    }
    val errors = closedParents.toSeq.map {
      case (cls, parent) =>
        s"${cls.id} extends or implements ${parent.id}, which the host does not declare open"
    } ++ badAllocations.keys.toSeq.map { cls =>
      s"the extension allocates ${cls.id}, which the host never allocates"
    }
    if (errors.nonEmpty)
      throw new LinkingException(
        errors.mkString("Extension crosses the host boundary:\n  ", "\n  ", "")
      )
  }

  protected def unknownSubclasses: Class => Boolean =
    liveExtension.fold(ReachabilityAnalysis.Result.noUnknownSubclasses) {
      ext => (cls: Class) =>
        ext.manifest.classByName.get(cls.name.id).exists { entry =>
          entry.isOpen || ext.manifest.hasSubclasses.contains(entry.name)
        }
    }

  final def isPhantom(name: nir.Global): Boolean =
    phantomTops.nonEmpty && phantomTops.contains(name.top)

  final def isHostOwned(name: nir.Global.Top): Boolean =
    liveExtension.exists(_.manifest.owns(name))

  /** Called by `newInfo` for every class, module and trait. */
  protected def onNewScope(info: ScopeInfo): Unit = {
    liveHost.foreach { host =>
      if (host.isOpen(info.name.id) && canBeOpen(info)) {
        openScopes += info
        pendingOpen += info
      }
      if (host.isCallable(info.name.id) && !info.attrs.isExtern)
        pendingCallable += info
    }
    liveExtension.foreach { ext =>
      if (ext.manifest.owns(info.name)) onHostScope(ext, info)
      else onExtensionScope(ext, info)
    }
  }

  /** Called once a class, module or trait definition is done. */
  protected def processPendingOpen(): Unit = {
    while (pendingOpen.nonEmpty) {
      val info = pendingOpen.remove(0)
      openType(info)
    }
    while (pendingCallable.nonEmpty) {
      val info = pendingCallable.remove(0)
      dispatchCallable(info)
    }
  }

  private val madeUpCalls = mutable.Set.empty[(nir.Global.Top, nir.Sig)]

  /** A call the host link makes up, so that the host has a dispatch table entry
   *  for a method extensions may call on a boundary type, although the host
   *  itself never calls it. Only implementations in boundary classes (callable
   *  or open ones) are kept for it: the host's other classes are never handed
   *  to extensions as such, so their entries stay empty. A real call of the
   *  same method on the same type makes it an ordinary call.
   */
  protected def makeUpCall(info: ScopeInfo, sig: nir.Sig)(implicit
      pos: nir.SourcePosition
  ): Unit =
    if (!info.calls.contains(sig)) {
      info.calls += sig
      madeUpCalls += ((info.name, sig))
      info.implementors.foreach { cls =>
        if (cls.allocated && isBoundaryClass(cls))
          cls.resolve(sig).foreach(reachGlobal)
      }
    }

  /** Turns a made-up call into a real one when code calls the same method on
   *  the same type, so that all of its targets are needed. Returns whether the
   *  call was made up.
   */
  protected def promoteMadeUpCall(scope: ScopeInfo, sig: nir.Sig): Boolean =
    madeUpCalls.nonEmpty && madeUpCalls.remove((scope.name, sig))

  /** Whether a newly allocated class needs no implementation of `sig`: only
   *  made-up calls ask for it, and the class is not a boundary class.
   */
  protected def skipMadeUpCall(cls: Class, sig: nir.Sig): Boolean =
    madeUpCalls.nonEmpty && !isBoundaryClass(cls) &&
      cls.linearized.forall { scope =>
        !scope.calls.contains(sig) || madeUpCalls.contains((scope.name, sig))
      }

  // Whether extensions may meet instances of the class: in a host link, open
  // and callable classes; in an extension link, where made-up calls come from
  // a chained extension's open types, every class
  private def isBoundaryClass(cls: Class): Boolean =
    liveHost.forall { host =>
      host.isCallable(cls.name.id) || host.isOpen(cls.name.id)
    }

  /** Every overridable method a callable class or trait declares counts as
   *  called on it, through a made-up call, so that the host has a vtable slot,
   *  an itable entry or a recorded direct target for it: an extension calls a
   *  method of a host type through the type that declares it
   *  (`live.HostDispatch`). Modules need none, as nothing overrides their
   *  methods.
   */
  private def dispatchCallable(info: ScopeInfo): Unit = info match {
    case cls: Class if cls.isModule => ()
    case _                          =>
      implicit val pos: nir.SourcePosition = info.position
      loaded.get(info.name).foreach { defns =>
        val declared = defns.valuesIterator
          .collect {
            case defn: nir.Defn.Define  => (defn.attrs, defn.name, false)
            case defn: nir.Defn.Declare => (defn.attrs, defn.name, true)
          }
          .collect {
            case (attrs, member @ nir.Global.Member(owner, sig), abstr)
                if owner == info.name && sig.isMethod && !sig.isPrivate &&
                  !sig.isStatic && !sig.isTraitInit && !attrs.isExtern &&
                  !isFinalMethod(owner, sig) =>
              (member, abstr)
          }
          .toSeq
          .sortBy(_._1.mangle)
        declared.foreach {
          case (member, abstr) =>
            // The declarations stay in what reachability passes on, so that
            // the second pass, after optimization, makes up the same calls
            if (abstr) reachGlobal(member)
            makeUpCall(info, member.sig)
        }
      }
  }

  /** Extension links: definitions of host methods the host exports are not
   *  followed.
   */
  protected def asHostDeclaration(defn: nir.Defn): nir.Defn =
    defn match {
      case defn: nir.Defn.Define
          if liveExtension.isDefined && isHostOwned(defn.name.top) &&
            !liveExtension.get.copiesMethod(defn.name) =>
        nir.Defn.Declare(defn.attrs, defn.name, defn.ty)(defn.pos)
      case _ => defn
    }

  /** Extension links: allocating a host class is allowed only if the host
   *  allocates it too, since host code may have devirtualized calls past it.
   */
  protected def checkHostAllocation(info: Class): Unit =
    liveExtension.foreach { ext =>
      ext.manifest.classByName.get(info.name.id).foreach { entry =>
        // An open class: the host dispatches virtually on it, so the
        // extension may allocate it too (a chained extension's classes)
        if (!entry.isAllocated && !entry.isOpen)
          badAllocations.getOrElseUpdate(
            info.name,
            mutable.LinkedHashSet.empty
          ) += stack.headOption.getOrElse(nir.Global.None)
      }
    }

  /** Extension links: the targets of calls on host classes that the host does
   *  not dispatch, by class and signature (`resolveHostCall`).
   */
  val hostTargets =
    mutable.Map.empty[(nir.Global.Top, nir.Sig), Seq[live.Dispatch.ClassCase]]

  private lazy val reachDispatch: Option[live.HostDispatch] =
    liveExtension.map { ext =>
      new live.HostDispatch(
        ext.manifest,
        () => infos(nir.Rt.Object.name).asInstanceOf[Class],
        hostTargets
      )
    }

  /** Extension links: a call on a host class or trait that the host does not
   *  dispatch, as its own calls of the method were inlined or devirtualized: no
   *  vtable slot or itable entry, no recorded target. The instances the call
   *  can meet are known when no extension can make others:
   *    - on a class, if the manifest lists all of its subclasses, none of them
   *      open: those of that subtree;
   *    - on a trait that is not open, if no open trait extends it and none of
   *      the host classes that implement it is open, so that no extension class
   *      implements it: those classes.
   *  Of these, the classes that the manifest says are allocated. The extension
   *  calls their implementations, exported methods or copies: directly if they
   *  all have the same one, otherwise by the receiver's class, which it tells
   *  by its RTTI. If a class has none it can find, the call does not link.
   */
  protected def resolveHostCall(scope: ScopeInfo, sig: nir.Sig)(implicit
      pos: nir.SourcePosition
  ): Unit = (liveExtension, reachDispatch, scope) match {
    case (Some(ext), Some(dispatch), cls: Class)
        if sig.isVirtual && ext.manifest.owns(cls.name) &&
          !hostTargets.contains((cls.name, sig)) &&
          dispatch.byHost(cls, sig).isEmpty =>
      closedSubtree(ext.manifest, cls.name.id)
        .foreach(dispatchByClass(ext, cls.name, sig, _))
    case (Some(ext), Some(dispatch), trt: Trait)
        if sig.isVirtual && ext.manifest.owns(trt.name) &&
          !ext.isOpen(trt.name) &&
          !hostTargets.contains((trt.name, sig)) &&
          dispatch.byHost(trt, sig).isEmpty =>
      closedImplementors(ext.manifest, trt.name.id)
        .foreach(dispatchByClass(ext, trt.name, sig, _))
    case _ => ()
  }

  /** Records how a call on `scope` reaches the implementations of the `classes`
   *  its receiver may be an instance of (`resolveHostCall`).
   */
  private def dispatchByClass(
      ext: ExtensionContext,
      scope: nir.Global.Top,
      sig: nir.Sig,
      classes: Seq[live.Manifest.ClassEntry]
  )(implicit pos: nir.SourcePosition): Unit = {
    val impls = classes
      .filter(_.isAllocated)
      .sortBy(_.name)
      .map(entry => (entry.name, implementation(ext, entry, sig)))
    if (impls.nonEmpty && impls.forall(_._2.isDefined)) {
      // One case per implementation, with the classes that run it, in the
      // order of their first class; the receiver's RTTI picks the case
      val byTarget =
        mutable.LinkedHashMap.empty[nir.Global.Member, Seq[String]]
      impls.foreach {
        case (name, Some(impl)) =>
          byTarget(impl) = byTarget.getOrElse(impl, Vector.empty) :+ name
        case _ => ()
      }
      val cases = byTarget.toSeq.map {
        case (impl, names) => live.Dispatch.ClassCase(names, impl)
      }
      hostTargets((scope, sig)) = cases
      cases.foreach(c => reachGlobal(c.target))
    }
  }

  /** The host classes that implement a trait, through their own traits, those
   *  traits' parents or their superclasses, unless one of them is open, which
   *  an extension may subclass, or an open trait extends it, which an extension
   *  may implement.
   */
  private def closedImplementors(
      manifest: live.Manifest,
      name: String
  ): Option[Seq[live.Manifest.ClassEntry]] =
    implementorsByTrait.getOrElseUpdate(name, findImplementors(manifest, name))

  private val implementorsByTrait =
    mutable.Map.empty[String, Option[Seq[live.Manifest.ClassEntry]]]

  private def findImplementors(
      manifest: live.Manifest,
      name: String
  ): Option[Seq[live.Manifest.ClassEntry]] = {
    val traitIs = mutable.Map.empty[String, Boolean]
    val classIs = mutable.Map.empty[String, Boolean]
    // A trait's parents, from its definition
    def parents(trt: String): Seq[String] =
      lookup(nir.Global.Top(trt), ignoreIfUnavailable = true) match {
        case Some(defn: nir.Defn.Trait) => defn.traits.map(_.id)
        case _                          => Nil
      }
    def isOrExtends(trt: String): Boolean = traitIs.get(trt) match {
      case Some(known) => known
      case None        =>
        val is = trt == name || parents(trt).exists(isOrExtends)
        traitIs(trt) = is
        is
    }
    def implements(entry: live.Manifest.ClassEntry): Boolean =
      classIs.get(entry.name) match {
        case Some(known) => known
        case None        =>
          val is = entry.traits.exists(isOrExtends) ||
            manifest.classByName.get(entry.parent).exists(implements)
          classIs(entry.name) = is
          is
      }
    val classes = manifest.classes.filter(implements)
    // An extension class may implement the trait through an open trait that
    // extends it, or as a subclass of an open class
    val open = classes.exists(_.isOpen) ||
      manifest.traits.exists(t => t.open && isOrExtends(t.name))
    if (open) None else Some(classes)
  }

  // A class and its subclasses, unless one of them is open
  private def closedSubtree(
      manifest: live.Manifest,
      name: String
  ): Option[Seq[live.Manifest.ClassEntry]] = {
    val out = mutable.UnrolledBuffer.empty[live.Manifest.ClassEntry]
    def loop(entry: live.Manifest.ClassEntry): Boolean =
      !entry.isOpen && {
        out += entry
        manifest.subclassesByName.getOrElse(entry.name, Nil).forall(loop)
      }
    manifest.classByName.get(name).filter(loop).map(_ => out.toSeq)
  }

  // The method that instances of a class run for `sig`: the first definition
  // in the class and its parents, unless one declares it abstract first
  private def implementation(
      ext: ExtensionContext,
      entry: live.Manifest.ClassEntry,
      sig: nir.Sig
  ): Option[nir.Global.Member] = {
    val member = nir.Global.Top(entry.name).member(sig)
    lookup(member, ignoreIfUnavailable = true) match {
      case Some(_: nir.Defn.Define) => Some(member)
      case Some(_)                  => None
      case None                     =>
        ext.manifest.classByName
          .get(entry.parent)
          .flatMap(implementation(ext, _, sig))
    }
  }

  /** Extension links: a field of a host class that the host's layout does not
   *  have, as no host code uses it. Code that uses it does not link.
   */
  protected def isFieldTheHostLacks(defn: nir.Defn): Boolean = defn match {
    case defn: nir.Defn.Var =>
      liveExtension.exists { ext =>
        ext.manifest.classByName
          .get(defn.name.top.id)
          .exists(!_.fieldNames.contains(defn.name.mangle))
      }
    case _ => false
  }

  /** Whether reachability should handle the static initializer and exported
   *  methods of this class (not the host's, in an extension link).
   */
  protected def ownsEntryPoints(name: nir.Global.Top): Boolean =
    liveExtension.forall(_.rootClasses.contains(name))

  private def canBeOpen(info: ScopeInfo): Boolean = info match {
    case cls: Class =>
      !cls.isModule && !cls.attrs.isExtern && !neverOpen(cls.name.id)
    case trt: Trait =>
      !trt.attrs.isExtern && !neverOpen(trt.name.id)
  }

  /** Signatures a subclass may override: the methods of every class and trait
   *  in the linearization, abstract ones included, except private, static and
   *  final ones. Sorted, for a deterministic reachability order.
   */
  protected def overridableSigs(infos: Seq[ScopeInfo]): Seq[nir.Sig] = {
    val sigs = mutable.Set.empty[nir.Sig]
    infos.foreach { info =>
      loaded.get(info.name).foreach { defns =>
        defns.valuesIterator.foreach {
          case defn: nir.Defn.Define  => add(info, defn.attrs, defn.name)
          case defn: nir.Defn.Declare => add(info, defn.attrs, defn.name)
          case _                      => ()
        }
      }
    }
    def add(info: ScopeInfo, attrs: nir.Attrs, name: nir.Global): Unit =
      name match {
        case nir.Global.Member(owner, sig)
            if sig.isMethod && !sig.isPrivate && !sig.isStatic &&
              !attrs.isExtern && !isFinalMethod(owner, sig) =>
          sigs += sig
        case _ => ()
      }
    sigs.toSeq.sortBy(_.mangle)
  }

  private def linearizationWithObject(info: ScopeInfo): Seq[ScopeInfo] =
    info match {
      case cls: Class => cls.linearized
      case trt: Trait =>
        trt.linearized ++ infos.get(nir.Rt.Object.name).collect {
          case obj: Class => obj
        }
    }

  private def openType(info: ScopeInfo): Unit = {
    implicit val pos: nir.SourcePosition = info.position
    val linearization = linearizationWithObject(info)
    val phantom = createPhantom(info)
    val sigs = overridableSigs(linearization)
    val sigSet = sigs.toSet

    // The declarations stay in what reachability passes on, so that the
    // second pass, after optimization, sees the same overridable signatures
    linearization.foreach { scope =>
      loaded.get(scope.name).foreach { defns =>
        defns.valuesIterator.toSeq.sortBy(_.name).foreach {
          case defn @ (_: nir.Defn.Define | _: nir.Defn.Declare) =>
            defn.name match {
              case member: nir.Global.Member if sigSet.contains(member.sig) =>
                reachGlobal(member)
              case _ => ()
            }
          case _ => ()
        }
      }
    }

    // Every overridable signature counts as called, so that vtable slots and
    // itable entries exist for extension code
    sigs.foreach(makeUpCall(info, _))

    // What an extension subclass inherits must exist out of line
    sigs.foreach { sig =>
      inheritedImpl(phantom, sig).foreach { impl =>
        reachGlobal(impl)
        boundaryRoots += impl
      }
    }
    info match {
      case cls: Class =>
        loaded.get(cls.name).foreach { defns =>
          defns.valuesIterator.toSeq.sortBy(_.name).foreach {
            case defn: nir.Defn.Define =>
              val member = defn.name.asInstanceOf[nir.Global.Member]
              if (member.sig.isCtor) {
                reachGlobal(member)
                boundaryRoots += member
              }
            // Extension subclasses lay out their fields after all of these
            case defn: nir.Defn.Var =>
              reachGlobal(defn.name)
            case _ => ()
          }
        }
      case _ => ()
    }
  }

  private def inheritedImpl(
      phantom: Class,
      sig: nir.Sig
  ): Option[nir.Global.Member] =
    phantom.parent
      .flatMap(_.resolve(sig))
      .orElse(phantom.linearized.collectFirst {
        case trt: Trait if trt.responds.contains(sig) => trt.responds(sig)
      })

  private def createPhantom(info: ScopeInfo): Class = {
    val name = nir.Global.Top(info.name.id + PhantomSuffix)
    val (parent, traits) = info match {
      case cls: Class => (cls, Nil)
      case trt: Trait =>
        (infos(nir.Rt.Object.name).asInstanceOf[Class], Seq(trt))
    }
    val phantom = new Class(
      nir.Attrs.None,
      name,
      Some(parent),
      traits,
      isModule = false
    )(info.position)
    phantomTops += name
    phantoms += phantom

    // Registered as the parents' subclass and the traits' implementor, as
    // `newInfo` does for classes, but never in `infos`
    def loopParent(parentInfo: Class): Unit = {
      parentInfo.implementors += phantom
      parentInfo.subclasses += phantom
      parentInfo.parent.foreach(loopParent)
      parentInfo.traits.foreach(loopTraits)
    }
    def loopTraits(traitInfo: Trait): Unit = {
      traitInfo.implementors += phantom
      traitInfo.traits.foreach(loopTraits)
    }
    loopParent(parent)
    traits.foreach(loopTraits)

    // What it does not override, it inherits, like any subclass
    phantom.responds ++= parent.responds
    phantom.linearized.foreach {
      case trt: Trait => phantom.defaultResponds ++= trt.responds
      case _          => ()
    }
    overridableSigs(linearizationWithObject(info)).foreach { sig =>
      phantom.responds(sig) = name.member(sig)
    }
    phantom.allocations = 1

    // Calls already reached on its supertypes reach what it inherits
    implicit val pos: nir.SourcePosition = info.position
    val calls = mutable.Set.empty[nir.Sig]
    phantom.linearized.foreach(scope => calls ++= scope.calls)
    calls.toSeq.sortBy(_.mangle).foreach { sig =>
      phantom.resolve(sig).foreach(reachGlobal)
    }
    phantom
  }

  /** Like `cleanup` for classes: drops what phantoms inherit but nothing
   *  reached.
   */
  protected def cleanupPhantoms(): Unit = {
    phantoms.foreach { phantom =>
      def prune(map: mutable.Map[nir.Sig, nir.Global.Member]): Unit =
        map.toArray.foreach {
          case (sig, impl) =>
            if (!isPhantom(impl) && !done.contains(impl)) map -= sig
        }
      prune(phantom.responds)
      prune(phantom.defaultResponds)
    }
    cleanupMadeUpCalls()
  }

  /** A class that is not a boundary class keeps no implementation of a method
   *  only made-up calls ask for. When it overrides a trait's default method,
   *  `cleanup` has dropped the override but not the default: drop the default
   *  too, so that its dispatch table entry is empty rather than the trait's.
   */
  private def cleanupMadeUpCalls(): Unit =
    if (madeUpCalls.nonEmpty) infos.valuesIterator.foreach {
      case cls: Class if !isBoundaryClass(cls) && !isPhantom(cls.name) =>
        cls.defaultResponds.keys.toArray.foreach { sig =>
          if (!cls.responds.contains(sig) && skipMadeUpCall(cls, sig)) {
            val overridden = Iterator
              .iterate(Option(cls))(_.flatMap(_.parent))
              .takeWhile(_.isDefined)
              .flatten
              .exists { c =>
                loaded
                  .get(c.name)
                  .flatMap(_.get(c.name.member(sig)))
                  .exists(_.isInstanceOf[nir.Defn.Define])
              }
            if (overridden) cls.defaultResponds -= sig
          }
        }
      case _ => ()
    }

  /** Whether a class is allocated by code this link knows about. */
  protected def isAllocatedForReal(cls: Class): Boolean =
    cls.allocated && !isPhantom(cls.name)

  private def onHostScope(ext: ExtensionContext, info: ScopeInfo): Unit =
    info match {
      case cls: Class =>
        ext.manifest.classByName.get(cls.name.id).foreach { entry =>
          // The extension lays out a host class as the host does
          entry.fields.foreach { field =>
            reachGlobal(nir.Unmangle.unmangleGlobal(field.name))(cls.position)
          }
          if (entry.isAllocated && !cls.allocated)
            reachAllocation(cls)(cls.position)
        }
      case _ => ()
    }

  private def onExtensionScope(ext: ExtensionContext, info: ScopeInfo): Unit = {
    val parents: Seq[ScopeInfo] = info match {
      case cls: Class => cls.parent.toSeq ++ cls.traits
      case trt: Trait => trt.traits
    }
    parents.foreach { parent =>
      if (ext.manifest.owns(parent.name) && !ext.isOpen(parent.name))
        closedParents.getOrElseUpdate(info.name, parent.name)
    }
    // A chained extension's own types are open to later extensions
    if (ext.config.isChained && canBeOpen(info)) {
      openScopes += info
      pendingOpen += info
    }
    info match {
      case cls: Class =>
        // Host code dispatches to every slot and itable entry the host knows
        // for the types this class extends: their implementations are needed
        // whether this extension calls them or not
        implicit val pos: nir.SourcePosition = cls.position
        val sigs = mutable.LinkedHashSet.empty[String]
        cls.linearized.foreach {
          case host: Class if ext.manifest.owns(host.name) =>
            sigs ++= ext.manifest.allSlots(host.name.id)
          case host: Trait if ext.manifest.owns(host.name) =>
            ext.manifest.traitByName
              .get(host.name.id)
              .foreach(sigs ++= _.methods)
          case _ => ()
        }
        sigs.foreach { sig =>
          cls.resolve(new nir.Sig(sig)).foreach(reachGlobal)
        }
      case _ => ()
    }
  }
}

private[scalanative] object LiveReach {
  final val PhantomSuffix = "$$Phantom"

  /** What a structural call in an extension is reported missing as
   *  (`Reach.UnsupportedFeature.StructuralCalls`).
   */
  val StructuralCallsFeature: nir.Global.Member =
    nir.Global
      .Top("scala.scalanative.runtime.UnsupportedFeature")
      .member(
        nir.Sig.Method(
          "structuralCalls",
          Seq(nir.Type.Unit),
          nir.Sig.Scope.PublicStatic
        )
      )

  /** Extension link inputs: its configuration and the host's manifest. */
  final class ExtensionContext(
      val config: LiveExecution.Extension,
      val manifest: live.Manifest
  ) {

    /** The classes of the extension's own class path entries. */
    lazy val rootClasses: Set[nir.Global.Top] =
      util.Scope { implicit scope =>
        config.roots.flatMap { path =>
          ClassPath(io.VirtualDirectory.real(path)).classes
        }.toSet -- manifest.classByName.keys.map(nir.Global.Top(_))
      }

    def isOpen(name: nir.Global.Top): Boolean =
      manifest.classByName.get(name.id).exists(_.isOpen) ||
        manifest.traitByName.get(name.id).exists(_.open)

    /** A method of a class of the host, or of an earlier extension of the
     *  chain, that it does not export: its optimizer inlined all of its calls,
     *  or nothing calls it there. This extension defines a copy of it, which
     *  its direct calls reach; virtual calls still go through the tables of the
     *  class's link. A method has no state of its own: the copy uses the host's
     *  modules, fields and classes as any extension code does. A module's
     *  constructor and static initializers run only where their class is
     *  defined.
     */
    def copiesMethod(name: nir.Global): Boolean = name match {
      case member @ nir.Global.Member(owner, sig) =>
        manifest.owns(owner) && !manifest.exportSet.contains(member.mangle) &&
          !sig.isClinit &&
          !(sig.isCtor && manifest.classByName.get(owner.id).exists(_.isModule))
      case _ => false
    }
  }

  /** Types that are never open: GC-special and runtime classes, and classes the
   *  runtime or the compiler treat as final.
   */
  def neverOpen(name: String): Boolean =
    name.startsWith("scala.scalanative.") ||
      name.startsWith("java.lang.ref.") ||
      finalClasses.contains(name)

  private val finalClasses = Set(
    "java.lang.String",
    "java.lang.Class",
    "java.lang.Boolean",
    "java.lang.Character",
    "java.lang.Byte",
    "java.lang.Short",
    "java.lang.Integer",
    "java.lang.Long",
    "java.lang.Float",
    "java.lang.Double",
    "java.lang.Void",
    "scala.runtime.BoxedUnit"
  )

  /** `java.lang.Object`'s final methods: NIR does not mark methods final. */
  private val objectFinalMethods = Set(
    "getClass",
    "notify",
    "notifyAll",
    "wait"
  )

  def isFinalMethod(owner: nir.Global.Top, sig: nir.Sig): Boolean =
    owner == nir.Rt.Object.name && (sig.unmangled match {
      case nir.Sig.Method(id, _, _) => objectFinalMethods.contains(id)
      case _                        => false
    })
}
