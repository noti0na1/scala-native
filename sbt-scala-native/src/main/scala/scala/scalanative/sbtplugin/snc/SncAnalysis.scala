package scala.scalanative.sbtplugin.snc

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, Paths}
import java.util.zip.ZipFile

import scala.collection.mutable
import scala.util.control.NonFatal

import scala.scalanative.nir
import scala.scalanative.nir.serialization.BinaryDeserializer

import xsbti.api._
import xsbti.{AnalysisCallback, Logger, VirtualFile}

/** Reports what `snc` produced to Zinc, from the NIR files it wrote, for an
 *  `snc` that does not stream Zinc's analysis.
 *
 *  It gives sbt what it needs without incremental compilation:
 *    - every source, so that Zinc stamps it and sees later changes;
 *    - every output file as a product of its source (NIR files as classes,
 *      TASTy files as local products), so that Zinc deletes them when their
 *      source is recompiled or removed;
 *    - a minimal API per class (kind, abstractness, all base classes, `main`
 *      methods), which is what sbt's test discovery reads;
 *    - main classes, for `run`.
 *
 *  There are no dependencies and no name hashes, so the plugin makes Zinc
 *  recompile everything whenever anything changes.
 */
private[scalanative] object SncAnalysis {

  private final case class NirClass(
      binaryName: String,
      defn: nir.Defn,
      file: Path,
      methods: Seq[nir.Sig.Unmangled]
  ) {
    def isModule: Boolean = defn.isInstanceOf[nir.Defn.Module]
    def isTrait: Boolean = defn.isInstanceOf[nir.Defn.Trait]
    def isAbstract: Boolean = isTrait || defn.attrs.isAbstract
    def sourceName: String =
      if (isModule) binaryName.stripSuffix("$") else binaryName
    def directParents: Seq[String] = defn match {
      case c: nir.Defn.Class  => (c.parent.toSeq ++ c.traits).map(_.id)
      case m: nir.Defn.Module => (m.parent.toSeq ++ m.traits).map(_.id)
      case t: nir.Defn.Trait  => t.traits.map(_.id)
      case _                  => Nil
    }
    def hasMainMethod(static: Boolean): Boolean = methods.exists {
      case nir.Sig.Method("main", Seq(nir.Type.Array(elem, _), ret), scope) =>
        scope.isStatic == static && scope.isPublic && isString(elem) &&
          ret == nir.Type.Unit
      case _ => false
    }
  }

  /** Reports `products`, the files that `snc` wrote under `outputDirectory` for
   *  `sources`, to `callback`.
   */
  def report(
      sources: Seq[(VirtualFile, Path)],
      products: Seq[Path],
      outputDirectory: Path,
      classpath: Seq[Path],
      workingDirectory: Path,
      callback: AnalysisCallback,
      log: Logger
  ): Unit = {
    sources.foreach { case (source, _) => callback.startSource(source) }
    if (sources.nonEmpty) {
      val classes = products
        .filter(_.getFileName.toString.endsWith(".nir"))
        .flatMap(readClass(_, outputDirectory, log))
      val byName = classes.map(c => c.binaryName -> c).toMap
      val sourceOfClass =
        new SourceLookup(sources, workingDirectory).forClasses(classes)
      def sourceOfFile(file: Path): VirtualFile = {
        // Foo.tasty, Foo$.nir and Foo$$Lambda$1.nir all belong to Foo's source.
        val relative = outputDirectory.relativize(file).toString
        val stem = relative.substring(0, relative.lastIndexOf('.'))
        owners(stem.replace(java.io.File.separatorChar, '.'))
          .flatMap(sourceOfClass.get)
          .headOption
          .getOrElse(sources.head._1)
      }

      val hierarchy = new Hierarchy(byName, classpath)
      try {
        val reportedMains = mutable.Set.empty[String]
        classes.foreach { cls =>
          val source =
            sourceOfClass.getOrElse(cls.binaryName, sourceOfFile(cls.file))
          if (isLocal(cls.binaryName))
            callback.generatedLocalClass(source, cls.file)
          else {
            callback.generatedNonLocalClass(
              source,
              cls.file,
              cls.binaryName,
              cls.sourceName
            )
            if (!isMirrorClass(cls, byName))
              callback.api(source, classLike(cls, hierarchy))
            val isMain =
              if (cls.isModule) cls.hasMainMethod(static = false)
              else cls.hasMainMethod(static = true)
            if (isMain && reportedMains.add(cls.sourceName))
              callback.mainClass(source, cls.sourceName)
          }
        }
      } finally hierarchy.close()

      products
        .filterNot(_.getFileName.toString.endsWith(".nir"))
        .foreach(file => callback.generatedLocalClass(sourceOfFile(file), file))
    }
    callback.apiPhaseCompleted()
    callback.dependencyPhaseCompleted()
  }

  private def readClass(
      file: Path,
      outputDirectory: Path,
      log: Logger
  ): Option[NirClass] = {
    val relative = outputDirectory.relativize(file).toString
    val binaryName = relative
      .stripSuffix(".nir")
      .replace(java.io.File.separatorChar, '.')
    try {
      val defns = deserialize(ByteBuffer.wrap(Files.readAllBytes(file)))
      val methods = defns.collect {
        case d: nir.Defn.Define  => d.name.sig.unmangled
        case d: nir.Defn.Declare => d.name.sig.unmangled
      }
      defns.collectFirst {
        case d: nir.Defn.Class if d.name.id == binaryName =>
          NirClass(binaryName, d, file, methods)
        case d: nir.Defn.Module if d.name.id == binaryName =>
          NirClass(binaryName, d, file, methods)
        case d: nir.Defn.Trait if d.name.id == binaryName =>
          NirClass(binaryName, d, file, methods)
      }
    } catch {
      case NonFatal(e) =>
        log.warn(() => s"Cannot read NIR file $file: $e")
        None
    }
  }

  private def deserialize(buffer: ByteBuffer): Seq[nir.Defn] =
    new BinaryDeserializer(buffer, nir.NIRSource.None).deserialize()

  /** Classes that Zinc should treat as local: lambdas, anonymous classes, and
   *  what the Scala Native compiler plugin generates for reflection.
   */
  private def isLocal(binaryName: String): Boolean =
    binaryName.contains("$$Lambda$") || binaryName.contains("$anon") ||
      binaryName.endsWith("ReflectivelyInstantiate$")

  /** The class `Foo` that holds static forwarders of `object Foo`, and has no
   *  companion class in the sources.
   */
  private def isMirrorClass(
      cls: NirClass,
      byName: Map[String, NirClass]
  ): Boolean = cls.defn match {
    case c: nir.Defn.Class =>
      byName.contains(cls.binaryName + "$") &&
        c.parent.forall(_.id == "java.lang.Object") && c.traits.isEmpty &&
        cls.methods.forall(_.sigScope.isStatic)
    case _ => false
  }

  /** `p.Foo` for `p.Foo$Bar$`, `p.Foo$$Lambda$1` and `p.Foo`. */
  def topLevelName(binaryName: String): String = {
    val lastDot = binaryName.lastIndexOf('.')
    val dollar = binaryName.indexOf('$', lastDot + 1)
    // A class named `$foo` keeps its leading dollar.
    if (dollar <= lastDot + 1) binaryName
    else binaryName.substring(0, dollar)
  }

  /** The classes that an output file named `binaryName` (its path under the
   *  output directory, without the extension and with dots) belongs to, nearest
   *  first: that class and its module class, then, for each `$` in the name
   *  from the last one, the class before it and its module class.
   *  `p/Foo$package.tasty`, the TASTy of the top-level definitions of
   *  `Foo.scala`, belongs to `p.Foo$package`; `p/Foo$$Lambda$1.nir` to `p.Foo$`
   *  or `p.Foo`.
   */
  def owners(binaryName: String): Seq[String] = {
    val start = binaryName.lastIndexOf('.') + 1
    // A class named `$foo` keeps its leading dollar.
    val dollars = (binaryName.length - 1 until start by -1)
      .filter(binaryName.charAt(_) == '$')
    (binaryName +: dollars.map(binaryName.substring(0, _)))
      .flatMap(name => Seq(name, name + "$"))
      .distinct
  }

  private def isString(tpe: nir.Type): Boolean = tpe match {
    case nir.Type.Ref(name, _, _) => name.id == "java.lang.String"
    case _                        => false
  }

  private def classLike(cls: NirClass, hierarchy: Hierarchy): ClassLike = {
    val definitionType =
      if (cls.isModule) DefinitionType.Module
      else if (cls.isTrait) DefinitionType.Trait
      else DefinitionType.ClassDef
    val parents: Array[Type] = hierarchy
      .ancestors(cls.binaryName)
      .map(name => Projection.of(EmptyType.of(), name): Type)
      .toArray
    val declared: Array[ClassDefinition] =
      if (cls.isModule && cls.hasMainMethod(static = false))
        Array(mainMethod)
      else Array.empty
    val structure = Structure.of(
      SafeLazy.strict(parents),
      SafeLazy.strict(declared),
      SafeLazy.strict(Array.empty[ClassDefinition])
    )
    ClassLike.of(
      cls.sourceName,
      Public.of(),
      modifiers(isAbstract = cls.isAbstract),
      Array.empty[Annotation],
      definitionType,
      SafeLazy.strict(EmptyType.of(): Type),
      SafeLazy.strict(structure),
      Array.empty[String],
      Array.empty[Type],
      true,
      Array.empty[TypeParameter]
    )
  }

  private def modifiers(isAbstract: Boolean): Modifiers =
    new Modifiers(isAbstract, false, false, false, false, false, false, false)

  private def named(name: String): Type = Projection.of(EmptyType.of(), name)

  /** `def main(args: Array[String]): Unit`, as sbt's discovery expects it. */
  private lazy val mainMethod: Def = {
    val stringArray =
      Parameterized.of(named("scala.Array"), Array(named("java.lang.String")))
    val args = MethodParameter.of(
      "args",
      stringArray,
      false,
      ParameterModifier.Plain
    )
    Def.of(
      "main",
      Public.of(),
      modifiers(isAbstract = false),
      Array.empty[Annotation],
      Array.empty[TypeParameter],
      Array(ParameterList.of(Array(args), false)),
      named("scala.Unit")
    )
  }

  /** Base classes of compiled classes, from their NIR and the class path's. */
  private final class Hierarchy(
      compiled: Map[String, NirClass],
      classpath: Seq[Path]
  ) {
    private val jars = mutable.Map.empty[Path, Option[ZipFile]]
    private val parentsCache = mutable.Map.empty[String, Seq[String]]

    /** All base classes and traits of `binaryName`, nearest first. */
    def ancestors(binaryName: String): Seq[String] = {
      val seen = mutable.LinkedHashSet.empty[String]
      val queue = mutable.Queue(parents(binaryName): _*)
      while (queue.nonEmpty) {
        val next = queue.dequeue()
        if (seen.add(next)) queue ++= parents(next)
      }
      seen.toSeq
    }

    def close(): Unit = jars.values.flatten.foreach(_.close())

    private def parents(binaryName: String): Seq[String] =
      parentsCache.getOrElseUpdate(
        binaryName,
        compiled.get(binaryName) match {
          case Some(cls) => cls.directParents
          case None      =>
            readFromClasspath(binaryName)
              .flatMap(_.collectFirst {
                case c: nir.Defn.Class if c.name.id == binaryName =>
                  (c.parent.toSeq ++ c.traits).map(_.id)
                case t: nir.Defn.Trait if t.name.id == binaryName =>
                  t.traits.map(_.id)
              })
              .getOrElse(Nil)
        }
      )

    private def readFromClasspath(binaryName: String): Option[Seq[nir.Defn]] = {
      val entry = binaryName.replace('.', '/') + ".nir"
      val roots = classpath.iterator
      var found: Option[Seq[nir.Defn]] = None
      while (found.isEmpty && roots.hasNext) {
        val root = roots.next()
        found =
          try read(root, entry).map(deserialize)
          catch { case NonFatal(_) => None }
      }
      found
    }

    private def read(root: Path, entry: String): Option[ByteBuffer] =
      if (Files.isDirectory(root)) {
        val file = root.resolve(entry)
        if (Files.isRegularFile(file))
          Some(ByteBuffer.wrap(Files.readAllBytes(file)))
        else None
      } else
        jar(root).flatMap { zip =>
          Option(zip.getEntry(entry)).map { e =>
            val in = zip.getInputStream(e)
            try ByteBuffer.wrap(readAll(in))
            finally in.close()
          }
        }

    private def jar(path: Path): Option[ZipFile] =
      jars.getOrElseUpdate(
        path,
        if (Files.isRegularFile(path))
          try Some(new ZipFile(path.toFile))
          catch { case NonFatal(_) => None }
        else None
      )

    private def readAll(in: java.io.InputStream): Array[Byte] = {
      val out = new java.io.ByteArrayOutputStream()
      val buffer = new Array[Byte](8192)
      var n = in.read(buffer)
      while (n >= 0) {
        out.write(buffer, 0, n)
        n = in.read(buffer)
      }
      out.toByteArray
    }
  }

  /** Finds the source of each class from its NIR position, which the Scala
   *  Native compiler plugin writes relative to a source directory (see
   *  `-P:scalanative:positionRelativizationPaths`) or to the working directory.
   */
  private final class SourceLookup(
      sources: Seq[(VirtualFile, Path)],
      workingDirectory: Path
  ) {
    private val byFileName: Map[String, Seq[(VirtualFile, Path)]] =
      sources.groupBy(_._2.getFileName.toString)

    def forClasses(classes: Seq[NirClass]): Map[String, VirtualFile] =
      classes.flatMap(c => sourceOf(c.defn.pos).map(c.binaryName -> _)).toMap

    private def sourceOf(pos: nir.SourcePosition): Option[VirtualFile] =
      pos.source match {
        case nir.SourceFile.Relative(pathString) =>
          val relative = Paths.get(pathString).normalize()
          val candidates =
            byFileName.getOrElse(relative.getFileName.toString, Nil)
          val resolved = workingDirectory.resolve(relative).normalize()
          candidates
            .find(_._2 == resolved)
            .orElse(candidates.find(_._2.endsWith(relative)))
            .orElse(if (candidates.size == 1) candidates.headOption else None)
            .map(_._1)
        case _ => None
      }
  }
}
