package scala.scalanative
package live

import scala.collection.mutable

import scala.scalanative.build.{Config, LiveExecution}
import scala.scalanative.linker.{ClassLoader, Link, ReachabilityAnalysis}
import scala.scalanative.util.Scope

/** A host link whose boundary is everything the class path defines, as a REPL's
 *  evaluator needs. Code that refers to something that does not link (a JDK
 *  class javalib lacks, a stub, an unsupported feature) does not fail the link
 *  and does not take whole classes out of the boundary:
 *
 *    - a method whose code needs it becomes a stub that throws
 *      `NoSuchMethodException` naming the method and what it needs, as a JVM
 *      throws a linkage error when such code runs;
 *    - a method whose signature needs it, or that is only a declaration or a
 *      stub, is left out of its class;
 *    - a class whose definition needs it (a missing parent or trait, a field of
 *      a missing type) is left out.
 *
 *  Reachability runs again after each round of changes, until it succeeds or a
 *  round changes nothing. The mixin forwarders of callable classes are left out
 *  where they decide nothing (`HostForwarders`), and come back when code calls
 *  them by name.
 */
private[scalanative] object HostStubs {

  def link(config: Config, entries: Seq[nir.Global], host: LiveExecution.Host)(
      implicit scope: Scope
  ): ReachabilityAnalysis = {
    val disk = ClassLoader.fromDisk(config)
    val stubbed = mutable.LinkedHashMap.empty[nir.Global.Member, String]
    val hiddenMembers = mutable.LinkedHashSet.empty[nir.Global.Member]
    val hiddenClasses = mutable.LinkedHashSet.empty[nir.Global.Top]
    // Bindings of another platform's libraries: code that uses them, behind
    // link-time conditions in ordinary programs, does not link here
    if (!config.targetsWindows)
      hiddenClasses ++= disk.classes.filter(_.id.startsWith(WindowsBindings))
    // Mixin forwarders of callable classes are left out, unless code calls
    // them by name (HostForwarders)
    val forwarders = HostForwarders.find(disk, callableClasses(disk, host))
    val withheld = mutable.Set.empty[nir.Global] ++= forwarders.keys
    val loader = new ClassLoader.Rewriting(
      disk,
      (top, defns) =>
        if (hiddenClasses.contains(top)) None
        else
          Some(defns.flatMap {
            case defn: nir.Defn.Define =>
              defn.name match {
                case m: nir.Global.Member if hiddenMembers.contains(m) => None
                case m: nir.Global.Member if withheld.contains(m)      => None
                case m: nir.Global.Member if stubbed.contains(m)       =>
                  Some(stub(defn, stubbed(m), defn.name.show))
                case _ => Some(defn)
              }
            case defn: nir.Defn.Declare =>
              defn.name match {
                case m: nir.Global.Member if hiddenMembers.contains(m) => None
                case _ => Some(defn)
              }
            case defn => Some(defn)
          })
    )
    var roots = callableRoots(disk, host).filter {
      case m: nir.Global.Member =>
        !hiddenClasses.contains(m.top) && !withheld.contains(m)
      case t: nir.Global.Top => !hiddenClasses.contains(t)
      case _                 => true
    }
    def attempt() = Link.withLoader(config, entries ++ roots, loader)
    def mentions(ty: nir.Type, top: nir.Global.Top): Boolean = ty match {
      case nir.Type.Function(args, ret) =>
        args.exists(mentions(_, top)) || mentions(ret, top)
      case nir.Type.Array(elem, _) => mentions(elem, top)
      case ref: nir.Type.RefKind   => ref.className == top
      case _                       => false
    }
    def signatureOf(m: nir.Global.Member): Option[nir.Type] =
      disk.load(m.owner.asInstanceOf[nir.Global.Top]).flatMap {
        _.collectFirst {
          case d: nir.Defn.Define if d.name == m  => d.ty
          case d: nir.Defn.Declare if d.name == m => d.ty
        }
      }
    def definedHere(m: nir.Global): Boolean = m match {
      case m: nir.Global.Member => signatureOf(m).isDefined
      case _                    => false
    }

    val knownMissing = mutable.Set.empty[nir.Global.Top]
    var result = attempt()
    var rounds = 1
    var progress = true
    while (!result.isSuccessful && rounds < MaxRounds && progress) {
      val failure = result.asInstanceOf[ReachabilityAnalysis.Failure]
      val before = (stubbed.size, hiddenMembers.size, hiddenClasses.size)
      val rootsBefore = roots.size
      val broken =
        failure.unreachable.map(u => (u.name, u.backtrace.map(_.name))) ++
          failure.unsupportedFeatures.map(u =>
            (nir.Global.None: nir.Global, u.backtrace.map(_.name))
          )
      // A left-out forwarder that code calls by name comes back
      val restored = broken.map(_._1).filter(withheld.contains)
      withheld --= restored
      broken.filterNot(b => restored.contains(b._1)).foreach {
        case (missing, backtrace) =>
          val missingTop = missing match {
            case nir.Global.None => None
            case name            => Some(name.top)
          }
          backtrace.headOption match {
            case None =>
              // A root, or reached for an open type's overridable signature:
              // a method that is only a declaration or a stub
              missing match {
                case m: nir.Global.Member if definedHere(m) =>
                  hiddenMembers += m
                case _ => ()
              }
              roots = roots.filterNot(_ == missing)
            case Some(top: nir.Global.Top) =>
              missing match {
                case m: nir.Global.Member if m.owner == top && definedHere(m) =>
                  hiddenMembers += m
                case _ => hiddenClasses += top
              }
            case Some(member: nir.Global.Member)
                if !(member.sig.isMethod || member.sig.isCtor) =>
              // A field of a type that does not link: its class goes
              hiddenClasses += member.top
            case Some(method: nir.Global.Member) =>
              val inSignature = missingTop.exists { t =>
                signatureOf(method).exists(mentions(_, t))
              }
              if (inSignature) hiddenMembers += method
              else if (!stubbed.contains(method))
                stubbed(method) =
                  missingTop.fold("an unsupported feature")(_ => missing.show)
            case Some(_) => ()
          }
      }
      // Reachability names one referrer per missing symbol: every other
      // method it loaded whose code refers to one becomes a stub too, rather
      // than one per round
      def absent(top: nir.Global.Top) =
        hiddenClasses.contains(top) || disk.load(top).isEmpty
      val missingNames = broken
        .map(_._1)
        .filter {
          case nir.Global.None                 => false
          case name if restored.contains(name) => false
          case t: nir.Global.Top               => absent(t)
          case _                               => true
        }
        .toSet
      if (missingNames.nonEmpty) {
        val absentTops = missingNames.collect {
          case top: nir.Global.Top => top
        }
        failure.defns.foreach {
          case d @ nir.Defn.Define(_, name: nir.Global.Member, ty, _, _)
              if !stubbed.contains(name) && !hiddenMembers.contains(name) =>
            firstReference(d, g => missingNames(g) || absentTops(g.top))
              .foreach { m =>
                if (absentTops.exists(t => mentions(ty, t)))
                  hiddenMembers += name
                else stubbed(name) = m.show
              }
          case _ => ()
        }
      }
      // Reachability names one referrer per missing type: leave out every
      // method whose signature, and every class whose field, needs one
      val missingTypes = broken.collect {
        case (top: nir.Global.Top, _) if disk.load(top).isEmpty => top
      }.toSet -- knownMissing
      if (missingTypes.nonEmpty) {
        knownMissing ++= missingTypes
        def needsMissing(ty: nir.Type) = missingTypes.exists(mentions(ty, _))
        disk.classes.foreach { top =>
          disk
            .load(top)
            .foreach(_.foreach {
              case d: nir.Defn.Define if needsMissing(d.ty) =>
                hiddenMembers += d.name
              case d: nir.Defn.Declare if needsMissing(d.ty) =>
                hiddenMembers += d.name
              case d: nir.Defn.Var if needsMissing(d.ty) =>
                hiddenClasses += top
              case _ => ()
            })
        }
      }
      roots = roots.filter {
        case m: nir.Global.Member =>
          !hiddenMembers.contains(m) && !hiddenClasses.contains(m.top)
        case t: nir.Global.Top => !hiddenClasses.contains(t)
        case _                 => true
      }
      progress =
        before != ((stubbed.size, hiddenMembers.size, hiddenClasses.size)) ||
          rootsBefore != roots.size || restored.nonEmpty
      if (progress) {
        config.logger.info(
          s"Live execution, round $rounds: ${stubbed.size} stubs, " +
            s"${hiddenMembers.size} methods and ${hiddenClasses.size} classes left out, for code that does not link"
        )
        result = attempt()
        rounds += 1
      }
    }
    if (forwarders.nonEmpty)
      config.logger.info(
        s"Live execution: ${forwarders.size - withheld.size} of ${forwarders.size} " +
          "mixin forwarders of callable classes kept"
      )
    if (stubbed.nonEmpty || hiddenMembers.nonEmpty || hiddenClasses.nonEmpty) {
      config.logger.info(
        s"Live execution: ${stubbed.size} host methods are stubs that throw, " +
          s"${hiddenMembers.size} methods and ${hiddenClasses.size} classes are left out (stubbed-methods.txt)"
      )
      writeList(config, stubbed, hiddenMembers, hiddenClasses)
    }
    result
  }

  /** An extension link whose code may refer to what does not link here, as a
   *  library's macro code may (a JDK class javalib lacks, another platform's
   *  bindings): the extension's own classes are rewritten as a host's boundary
   *  is. A method whose code needs such a symbol, or an unsupported feature,
   *  becomes a stub that throws `NoSuchMethodException` naming it and what it
   *  needs; a method whose signature needs a missing type is left out; a class
   *  whose definition needs one (a missing parent, a field of a missing type)
   *  is left out. The host's classes are declarations in an extension link and
   *  are never rewritten. Only the classes reachability loaded are searched for
   *  signatures that need a missing type, not the whole class path.
   */
  def linkExtension(
      config: Config,
      entries: Seq[nir.Global],
      ext: LiveExecution.Extension
  )(implicit scope: Scope): ReachabilityAnalysis = {
    val disk = ClassLoader.fromDisk(config)
    val manifest = ManifestChain.manifest(ext)
    def isHost(top: nir.Global.Top) = manifest.owns(top)
    // The extension's own methods, and its copies of host methods
    // (`LiveReach.ExtensionContext.copiesMethod`), may become stubs
    val context = new linker.LiveReach.ExtensionContext(ext, manifest)
    def isOwn(m: nir.Global.Member) =
      !isHost(m.top) || context.copiesMethod(m)
    // Why code that uses `missing` does not link
    def why(missing: nir.Global): String = missing match {
      case top: nir.Global.Top if isHost(top) =>
        readable(top) + ", which the host never allocates"
      case m: nir.Global.Member if isHost(m.top) && m.sig.isField =>
        readable(m) + ", which the host does not keep"
      case _ => readable(missing)
    }
    val stubbed = mutable.LinkedHashMap.empty[nir.Global.Member, String]
    val hiddenMembers = mutable.LinkedHashSet.empty[nir.Global.Member]
    val hiddenClasses = mutable.LinkedHashSet.empty[nir.Global.Top]
    // Classes that no class path entry has, which another class path may have
    val absentClasses = mutable.SortedSet.empty[String]
    // Another platform's bindings, behind link-time conditions
    def otherPlatform(top: nir.Global.Top) =
      !config.targetsWindows && top.id.startsWith(WindowsBindings)
    // Structural calls, which extensions do not support
    // (`Reach.UnsupportedFeature.StructuralCalls`)
    def structuralCall(defn: nir.Defn.Define) =
      defn.insts.exists {
        case nir.Inst.Let(_, _: nir.Op.Dynmethod, _) => true
        case _                                       => false
      }
    val loader = new ClassLoader.Rewriting(
      disk,
      (top, defns) =>
        if (isHost(top))
          Some(defns.map {
            case defn: nir.Defn.Define =>
              defn.name match {
                case m: nir.Global.Member if stubbed.contains(m) =>
                  stub(defn, stubbed(m), readable(m))
                case m: nir.Global.Member
                    if context.copiesMethod(m) && structuralCall(defn) =>
                  stubbed(m) = StructuralCalls
                  stub(defn, StructuralCalls, readable(m))
                case _ => defn
              }
            case defn => defn
          })
        else if (hiddenClasses.contains(top) || otherPlatform(top)) None
        else
          Some(defns.flatMap {
            case defn: nir.Defn.Define =>
              defn.name match {
                case m: nir.Global.Member if hiddenMembers.contains(m) => None
                case m: nir.Global.Member if stubbed.contains(m)       =>
                  Some(stub(defn, stubbed(m), readable(m)))
                case m: nir.Global.Member if structuralCall(defn) =>
                  stubbed(m) = StructuralCalls
                  Some(stub(defn, StructuralCalls, readable(m)))
                case _ => Some(defn)
              }
            case defn: nir.Defn.Declare =>
              defn.name match {
                case m: nir.Global.Member if hiddenMembers.contains(m) => None
                case _ => Some(defn)
              }
            case defn => Some(defn)
          })
    )
    var roots = entries
    def attempt() = Link.withLoader(config, roots, loader)
    def mentions(ty: nir.Type, top: nir.Global.Top): Boolean = ty match {
      case nir.Type.Function(args, ret) =>
        args.exists(mentions(_, top)) || mentions(ret, top)
      case nir.Type.Array(elem, _) => mentions(elem, top)
      case ref: nir.Type.RefKind   => ref.className == top
      case _                       => false
    }
    def signatureOf(m: nir.Global.Member): Option[nir.Type] =
      disk.load(m.owner.asInstanceOf[nir.Global.Top]).flatMap {
        _.collectFirst {
          case d: nir.Defn.Define if d.name == m  => d.ty
          case d: nir.Defn.Declare if d.name == m => d.ty
        }
      }

    // The extension's methods that call a method of a host type that the
    // host does not dispatch (`HostDispatch`), with the method
    def undispatchedCalls(
        result: ReachabilityAnalysis.Result
    ): Seq[(nir.Global.Member, String)] = {
      val dispatch = HostDispatch(manifest)(result)
      def undispatched(ty: nir.Type, sig: nir.Sig): Option[String] =
        ty match {
          case ref: nir.Type.RefKind if isHost(ref.className) =>
            result.infos.get(ref.className) match {
              case Some(scope: linker.ScopeInfo) =>
                try { dispatch(scope, sig); None }
                catch {
                  case _: linker.LinkingException =>
                    Some(
                      readable(scope.name.member(sig)) +
                        ", which the host does not dispatch"
                    )
                }
              case _ => None
            }
          case _ => None
        }
      result.defns.flatMap {
        case nir.Defn.Define(_, name: nir.Global.Member, _, insts, _)
            if isOwn(name) =>
          insts.iterator
            .collect {
              case nir.Inst.Let(_, nir.Op.Method(obj, sig), _) =>
                undispatched(obj.ty, sig)
            }
            .collectFirst { case Some(why) => name -> why }
        case _ => None
      }
    }

    val knownMissing = mutable.Set.empty[nir.Global.Top]
    var result = attempt()
    var rounds = 1
    var progress = true
    var again = true
    while (again) {
      again = false
      while (!result.isSuccessful && rounds < MaxRounds && progress) {
        val failure = result.asInstanceOf[ReachabilityAnalysis.Failure]
        val before = (stubbed.size, hiddenMembers.size, hiddenClasses.size)
        val rootsBefore = roots.size
        val broken =
          failure.unreachable.map(u => (u.name, u.backtrace.map(_.name))) ++
            failure.unsupportedFeatures.map(u =>
              (nir.Global.None: nir.Global, u.backtrace.map(_.name))
            )
        broken.foreach {
          case (nir.Global.None, _) => ()
          case (missing, _)         =>
            val top = missing.top
            if (!isHost(top) && !otherPlatform(top) && disk.load(top).isEmpty)
              absentClasses += top.id
        }
        broken.foreach {
          case (missing, backtrace) =>
            val missingTop = missing match {
              case nir.Global.None => None
              case name            => Some(name.top)
            }
            backtrace.headOption match {
              case None =>
                // A root that does not exist
                roots = roots.filterNot(_ == missing)
              case Some(top: nir.Global.Top) if !isHost(top) =>
                missing match {
                  case m: nir.Global.Member
                      if m.owner == top && signatureOf(m).isDefined =>
                    hiddenMembers += m
                  case _ => hiddenClasses += top
                }
              case Some(member: nir.Global.Member)
                  if !isHost(member.top) &&
                    !(member.sig.isMethod || member.sig.isCtor) =>
                hiddenClasses += member.top
              case Some(method: nir.Global.Member) if isOwn(method) =>
                // The host's types are there, whatever their members
                val inSignature = missingTop.filterNot(isHost).exists { t =>
                  signatureOf(method).exists(mentions(_, t))
                }
                if (inSignature && !isHost(method.top)) hiddenMembers += method
                else if (!stubbed.contains(method))
                  stubbed(method) =
                    missingTop.fold("an unsupported feature")(_ => why(missing))
              case Some(_) => ()
            }
        }
        // Classes that do not load or are left out; not the closed host types
        // extension classes extend, which only those classes need
        def absent(top: nir.Global.Top) =
          hiddenClasses.contains(top) || otherPlatform(top) ||
            disk.load(top).isEmpty
        val missingTypes = broken.collect {
          case (top: nir.Global.Top, _) if absent(top) => top
        }.toSet -- knownMissing
        // Reachability names one referrer per missing symbol: every other
        // method of the extension it loaded whose code refers to one becomes a
        // stub too, rather than one per round
        val missingNames = broken
          .map(_._1)
          .filter {
            case nir.Global.None   => false
            case t: nir.Global.Top => absent(t)
            case _                 => true
          }
          .toSet
        if (missingNames.nonEmpty) {
          val absentTops = missingNames.collect {
            case top: nir.Global.Top => top
          }
          failure.defns.foreach {
            case d @ nir.Defn.Define(_, name: nir.Global.Member, ty, _, _)
                if isOwn(name) && !stubbed.contains(name) &&
                  !hiddenMembers.contains(name) =>
              firstReference(d, g => missingNames(g) || absentTops(g.top))
                .foreach { m =>
                  if (absentTops.exists(t => mentions(ty, t)) &&
                      !isHost(name.top))
                    hiddenMembers += name
                  else stubbed(name) = why(m)
                }
            case _ => ()
          }
        }
        // Reachability names one referrer per missing type: leave out every
        // method of the classes it loaded whose signature, and every class
        // whose field, needs one
        if (missingTypes.nonEmpty) {
          knownMissing ++= missingTypes
          def needsMissing(ty: nir.Type) = missingTypes.exists(mentions(ty, _))
          failure.defns.iterator
            .map(_.name.top)
            .toSet
            .filterNot(isHost)
            .foreach { top =>
              disk
                .load(top)
                .foreach(_.foreach {
                  case d: nir.Defn.Define if needsMissing(d.ty) =>
                    hiddenMembers += d.name
                  case d: nir.Defn.Declare if needsMissing(d.ty) =>
                    hiddenMembers += d.name
                  case d: nir.Defn.Var if needsMissing(d.ty) =>
                    hiddenClasses += top
                  case _ => ()
                })
            }
        }
        roots = roots.filter {
          case m: nir.Global.Member =>
            !hiddenMembers.contains(m) && !hiddenClasses.contains(m.top)
          case t: nir.Global.Top => !hiddenClasses.contains(t)
          case _                 => true
        }
        progress =
          before != ((stubbed.size, hiddenMembers.size, hiddenClasses.size)) ||
            rootsBefore != roots.size
        if (progress) {
          config.logger.info(
            s"Live execution, round $rounds: ${stubbed.size} stubs, " +
              s"${hiddenMembers.size} methods and ${hiddenClasses.size} classes left out, for code that does not link"
          )
          result = attempt()
          rounds += 1
        }
      }
      // Calls on host types that the host does not dispatch (a method of a
      // host class outside its callable set): the methods that make them become
      // stubs, and reachability runs again
      if (result.isSuccessful && rounds < MaxRounds) {
        val undispatched =
          undispatchedCalls(result.asInstanceOf[ReachabilityAnalysis.Result])
            .filterNot { case (m, _) => stubbed.contains(m) }
        if (undispatched.nonEmpty) {
          stubbed ++= undispatched
          config.logger.info(
            s"Live execution, round $rounds: ${stubbed.size} stubs, " +
              s"${undispatched.size} for calls the host does not dispatch"
          )
          result = attempt()
          rounds += 1
          progress = true
          again = true
        }
      }
    }
    if (stubbed.nonEmpty || hiddenMembers.nonEmpty || hiddenClasses.nonEmpty) {
      config.logger.info(
        s"Live execution: ${stubbed.size} extension methods are stubs that throw, " +
          s"${hiddenMembers.size} methods and ${hiddenClasses.size} classes are left out (stubbed-methods.txt)"
      )
      writeList(config, stubbed, hiddenMembers, hiddenClasses)
    }
    writeMissingClasses(config, absentClasses)
    result
  }

  /** `stubbed-methods.txt` in the link's work directory. */
  private def writeList(
      config: Config,
      stubbed: collection.Map[nir.Global.Member, String],
      hiddenMembers: Iterable[nir.Global.Member],
      hiddenClasses: Iterable[nir.Global.Top]
  ): Unit = {
    val list = config.workDir.resolve("stubbed-methods.txt")
    java.nio.file.Files.createDirectories(config.workDir)
    val lines =
      stubbed.toSeq.map { case (m, why) => "stub\t" + m.show + "\t" + why } ++
        hiddenMembers.toSeq.map(m => "left out\t" + m.show) ++
        hiddenClasses.toSeq.map(c => "left out\t" + c.show)
    java.nio.file.Files.write(
      list,
      lines.sorted
        .mkString("", "\n", "\n")
        .getBytes(java.nio.charset.StandardCharsets.UTF_8)
    )
  }

  /** `missing-classes.txt` in the link's work directory: the classes that the
   *  stubs and the code left out need and that no class path entry has, one
   *  name per line. A cache of links uses it to tell when a class path that has
   *  one of them makes a cached link stale; it is removed when there are none.
   */
  private def writeMissingClasses(
      config: Config,
      classes: collection.Set[String]
  ): Unit = {
    val list = config.workDir.resolve(MissingClassesFile)
    if (classes.isEmpty) java.nio.file.Files.deleteIfExists(list)
    else {
      java.nio.file.Files.createDirectories(config.workDir)
      java.nio.file.Files.write(
        list,
        classes.toSeq.sorted
          .mkString("", "\n", "\n")
          .getBytes(java.nio.charset.StandardCharsets.UTF_8)
      )
    }
  }

  final val MissingClassesFile = "missing-classes.txt"

  /** The first symbol `defn`'s code refers to that `wanted` accepts. */
  private def firstReference(
      defn: nir.Defn.Define,
      wanted: nir.Global => Boolean
  ): Option[nir.Global] = {
    var found: Option[nir.Global] = None
    def check(g: nir.Global): Unit =
      if (found.isEmpty && wanted(g)) found = Some(g)
    val traverse = new nir.Traverse {
      override def onVal(value: nir.Val): Unit = value match {
        case nir.Val.Global(name, _) => check(name)
        case nir.Val.ClassOf(name)   => check(name)
        case _                       => super.onVal(value)
      }
      override def onType(ty: nir.Type): Unit = ty match {
        case ref: nir.Type.RefKind => check(ref.className)
        case _                     => super.onType(ty)
      }
      override def onOp(op: nir.Op): Unit = {
        op match {
          case nir.Op.Classalloc(name, _) => check(name)
          case nir.Op.Module(name)        =>
            check(name)
            // What the instruction calls, which reachability reports missing
            // when `name` is not a module
            check(name.member(nir.Sig.Generated("load")))
          case nir.Op.Fieldload(_, _, name)     => check(name)
          case nir.Op.Fieldstore(_, _, name, _) => check(name)
          case nir.Op.Field(_, name)            => check(name)
          case _                                => ()
        }
        super.onOp(op)
      }
    }
    traverse.onInsts(defn.insts)
    found
  }

  /** A symbol as people read it: `java.lang.Class.getMethod(java.lang.String,
   *  java.lang.Class[]): java.lang.reflect.Method`.
   */
  private def readable(g: nir.Global): String = {
    def tpe(t: nir.Type): String = t match {
      case nir.Type.Ref(top, _, _) => top.id
      case nir.Type.Array(elem, _) => tpe(elem) + "[]"
      case other                   => other.show
    }
    g match {
      case nir.Global.Top(id)            => id
      case nir.Global.Member(owner, sig) =>
        sig.unmangled match {
          case nir.Sig.Method(id, types, _) =>
            s"${owner.id}.$id(${types.init.map(tpe).mkString(", ")}): ${tpe(types.last)}"
          case nir.Sig.Ctor(types) =>
            s"${owner.id}.<init>(${types.map(tpe).mkString(", ")})"
          case nir.Sig.Field(id, _) => s"${owner.id}.$id"
          case _                    => g.show
        }
      case _ => g.show
    }
  }

  private final val StructuralCalls =
    "structural calls (reflectiveSelectable), which extensions do not support"

  private final val MaxRounds = 40
  private final val WindowsBindings = "scala.scalanative.windows."

  /** The classes matching the callable patterns, except lambdas and anonymous
   *  classes, which only the code around them refers to, and the classes whose
   *  values are primitives, whose methods are intrinsics.
   */
  private def callableClasses(
      loader: ClassLoader,
      host: LiveExecution.Host
  ): Seq[nir.Global.Top] =
    loader.classes.toSeq
      .filter { top =>
        host.isCallable(top.id) && !PrimitiveValueClasses.contains(top.id) &&
        !top.id.contains("$$Lambda$") && !top.id.contains("$$anon")
      }
      .sortBy(_.id)

  private val PrimitiveValueClasses: Set[String] =
    (Seq("Unit", "Boolean", "Char", "Byte", "Short", "Int", "Long", "Float") ++
      Seq("Double", "Nothing", "Null", "Array", "Any", "AnyVal", "AnyRef"))
      .map("scala." + _)
      .toSet

  /** Every callable class and all of its methods. */
  private def callableRoots(
      loader: ClassLoader,
      host: LiveExecution.Host
  ): Seq[nir.Global] =
    callableClasses(loader, host)
      .flatMap { top =>
        val defns = loader.load(top).toSeq.flatten
        // Some NIR files define nothing (companions the compiler dropped);
        // extern objects are C functions, which code that calls them reaches
        // (with their `@link`, which may name another platform's library)
        val defined = defns.exists {
          case d: nir.Defn.Class  => d.name == top && !d.attrs.isExtern
          case d: nir.Defn.Module => d.name == top && !d.attrs.isExtern
          case d: nir.Defn.Trait  => d.name == top && !d.attrs.isExtern
          case _                  => false
        }
        // Not private methods, which only the class's own code calls, nor the
        // JVM's lazy-val handles: code that needs them reaches them
        val methods = defns.collect {
          case defn: nir.Defn.Define
              if !defn.attrs.isStub && !defn.name.sig.isPrivate &&
                !(defn.name.sig.unmangled match {
                  case nir.Sig.Method(id, _, _) => id.endsWith("$lzyHandle")
                  case _                        => false
                }) =>
            defn.name
        }
        if (defined) top +: methods.sortBy(_.mangle) else Nil
      }

  /** A method that throws `NoSuchMethodException` naming itself and what it
   *  needs.
   */
  private def stub(
      defn: nir.Defn.Define,
      missing: String,
      name: String
  ): nir.Defn.Define = {
    implicit val pos: nir.SourcePosition = defn.pos
    implicit val fresh: nir.Fresh = nir.Fresh()
    implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
    val nir.Type.Function(argtys, _) = defn.ty
    val params = argtys.map(ty => nir.Val.Local(fresh(), ty))
    val message = s"$name does not link in this program: it needs $missing"
    nir.Defn.Define(
      defn.attrs,
      defn.name,
      defn.ty,
      Seq(
        nir.Inst.Label(fresh(), params),
        nir.Inst.Let(
          nir.Op.Call(
            codegen.Lower.throwNoSuchMethodStringTy,
            codegen.Lower.throwNoSuchMethodVal,
            Seq(nir.Val.Null, nir.Val.String(message))
          ),
          nir.Next.None
        ),
        nir.Inst.Unreachable(nir.Next.None)
      )
    )
  }
}
