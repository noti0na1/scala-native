package scala.scalanative
package codegen

import scala.collection.mutable

import scalanative.linker.{Class, ReachabilityAnalysis, ScopeInfo, Trait}

private[scalanative] class Metadata(
    val analysis: ReachabilityAnalysis.Result,
    val buildConfig: build.Config,
    proxies: Seq[nir.Defn]
)(implicit val platform: PlatformInfo) {
  def config: build.NativeConfig = buildConfig.compilerConfig
  private implicit def self: Metadata = this

  /** How this link's live-execution role changes the metadata. */
  val live: LiveMetadata = LiveMetadata(this)

  final val usesLockWords = platform.isMultithreadingEnabled
  val lockWordType = if (usesLockWords) Some(nir.Type.Ptr) else None
  private[codegen] val lockWordVals = lockWordType.map(_ => nir.Val.Null).toList

  val layouts = new CommonMemoryLayouts()
  val globalSections = new GlobalSections
  val rtti = mutable.Map.empty[linker.Info, RuntimeTypeInformation]
  val vtable = mutable.Map.empty[linker.Class, VirtualTable]
  val itable = mutable.Map.empty[linker.Class, ITable]
  val layout = mutable.Map.empty[linker.Class, FieldLayout]

  // Superclasses from the root, then the class itself, see lineage
  private val lineages = mutable.Map.empty[Class, Vector[Class]]

  /** Every class with metadata. In an extension link, the host's classes too,
   *  whose layouts and dispatch tables its code uses.
   */
  val allClasses: Seq[Class] = initClasses()

  /** The classes this link emits RTTI for: in an extension link, its own. */
  val classes: Seq[Class] = live match {
    case ext: LiveMetadata.Extension => allClasses.filterNot(ext.isHostOwned)
    case _                           => allClasses
  }
  val allTraits: Seq[Trait] = analysis.infos.valuesIterator
    .collect { case info: Trait => info }
    .toIndexedSeq
    .sortBy(_.name.mangle)

  /** The traits this link emits RTTI for: in an extension link, its own. */
  val traits: Seq[Trait] = live match {
    case ext: LiveMetadata.Extension => allTraits.filterNot(ext.isHostOwned)
    case _                           => allTraits
  }
  private val traitKeys: Map[Trait, Int] =
    allTraits.map(trt => trt -> StableHash.traitKey(trt)).toMap

  checkDynsigKeys()
  initTraitMetadata()
  initClassMetadata()

  // Traits some class does not store in the itable slot where their lookup
  // starts, with the largest number of slots between. Computed before
  // lowering, which reads it from several threads.
  private val displacedTraits: Map[Trait, Int] =
    itable.valuesIterator
      .flatMap(_.displaced)
      .toSeq
      .groupBy(_._1)
      .map { case (trt, all) => trt -> all.map(_._2).max }
  // Traits implemented by some class whose itable shifts the trait keys
  private val shiftedTraits: Set[Trait] =
    classes.iterator
      .filter(itable(_).shift != 0)
      .flatMap(interfaces(_))
      .toSet

  /** Proxies of reflectively called methods, registered at startup */
  lazy val reflectiveProxies: nir.Defn.Const =
    ReflectiveProxyTable.defn(proxies)

  /** Superclasses of the class from the root, then the class itself */
  def lineage(cls: Class): Vector[Class] = lineages(cls)

  /** Number of superclasses: 0 for java.lang.Object and primitives */
  def superclassCount(cls: Class): Int = lineage(cls).size - 1

  /** Whether `cls` is `ancestor` or one of its subclasses: `ancestor` has
   *  `depth` superclasses.
   */
  def isAt(cls: Class, depth: Int, ancestor: nir.Global.Top): Boolean =
    lineage(cls).lift(depth).exists(_.name == ancestor)

  /** WeakReference, SoftReference or PhantomReference for reference objects:
   *  java.lang.ref.Reference, a direct subclass of Object, is extended only by
   *  them.
   */
  def referenceKind(cls: Class): Option[nir.Global.Top] =
    if (!isAt(cls, 1, FieldLayout.Reference)) None
    else lineage(cls).lift(2).map(_.name)

  /** Every class of the program stores `trt` in the itable slot where its
   *  lookup starts, so a single probe decides any lookup. A whole-program fact
   *  used by generated code only: RTTI does not record it.
   */
  def alwaysAtHomeSlot(trt: Trait): Boolean =
    live.knowsItables(trt) && !displacedTraits.contains(trt)

  /** Largest number of slots, in any class of the program, between the slot
   *  where the lookup of `trt` starts and the one holding it. A whole-program
   *  fact used by generated code.
   */
  def maxDisplacement(trt: Trait): Int =
    if (!live.knowsItables(trt)) Int.MaxValue
    else displacedTraits.getOrElse(trt, 0)

  /** No class of the program shifts the key of `trt` in its itable, so lookups
   *  start at key & mask. A whole-program fact used by generated code.
   */
  def neverShifted(trt: Trait): Boolean =
    live.knowsItables(trt) && !shiftedTraits.contains(trt)

  /** The methods of a trait in itable order: the host's order for a host trait
   *  in an extension link.
   */
  def traitMethods(trt: Trait): Seq[nir.Sig] = live match {
    case ext: LiveMetadata.Extension if ext.isHostOwned(trt) =>
      ext.hostTraitMethods(trt)
    case _ => trt.methods
  }

  /** Whether a module's instance is a constant: the host's answer for a host
   *  module in an extension link.
   */
  def isConstantModule(cls: Class): Boolean = live match {
    case ext: LiveMetadata.Extension if ext.isHostOwned(cls) =>
      ext.manifestClass(cls).isConstantModule
    case _ => cls.isConstantModule(analysis)
  }

  /** Key of a trait in itables, see StableHash.traitKey */
  def traitKey(trt: Trait): Int = traitKeys(trt)

  /** Traits implemented by the class or trait, ordered by name */
  def interfaces(info: ScopeInfo): Seq[Trait] =
    info.linearized
      .collect { case trt: Trait if trt != info => trt }
      .distinct
      .sortBy(_.name.mangle)

  // Classes in pre-order of the class hierarchy, primitive types first
  private def initClasses(): Seq[Class] = {
    val out = mutable.UnrolledBuffer.empty[Class]

    def loop(node: Class, parentLineage: Vector[Class]): Unit = {
      out += node
      val lineage = parentLineage :+ node
      lineages(node) = lineage
      // Not the phantom subclasses of open classes (live execution), which
      // stand for extension classes and get no metadata
      node.subclasses
        .filter(sub =>
          sub.parent.contains(node) && !analysis.isPhantom(sub.name)
        )
        .toSeq
        .sortBy(_.name.mangle)
        .foreach(loop(_, lineage))
    }

    nir.Rt.PrimitiveTypes.foreach { name =>
      loop(analysis.infos(name).asInstanceOf[Class], Vector.empty)
    }
    loop(analysis.infos(nir.Rt.Object.name).asInstanceOf[Class], Vector.empty)
    out.toSeq
  }

  /* Reflective calls look methods up by a hash of their signature, so two
   * signatures with the same key could dispatch to a wrong method.
   */
  private def checkDynsigKeys(): Unit = {
    val sigs = (analysis.dynsigs ++ proxies.map(_.name).collect {
      case name: nir.Global.Member => name.sig
    }).distinct
    sigs.groupBy(StableHash.dynsigKey).foreach {
      case (key, colliding) if colliding.size > 1 =>
        throw new build.BuildException(
          s"Reflective method signatures ${colliding.map(_.show).mkString(", ")} share the dispatch key $key"
        )
      case _ => ()
    }
  }

  def initClassMetadata(): Unit = {
    allClasses.foreach { node =>
      vtable(node) = new VirtualTable(node)
      layout(node) = new FieldLayout(node)
      // A host class's itable is the host's: an extension never emits it
      if (!live.isHostOwned(node)) itable(node) = ITable.build(node)
      rtti(node) = new RuntimeTypeInformation(node)
    }
  }

  def initTraitMetadata(): Unit = {
    allTraits.foreach { node =>
      rtti(node) = new RuntimeTypeInformation(node)
    }
  }
}
