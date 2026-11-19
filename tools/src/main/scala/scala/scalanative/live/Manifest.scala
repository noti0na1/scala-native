package scala.scalanative
package live

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.collection.mutable

import scala.scalanative.build.{BuildException, Sha1}

/** What an extension linker needs to know about a host: written when a host
 *  links, read when an extension links against it.
 *
 *  A binary file: the magic `SNMF`, the format version, the payload's SHA-1 in
 *  hex (40 ASCII bytes), then the payload. The hash is embedded in the host
 *  (`__scalanative_ext_host_hash`) and in every extension, and the loader
 *  refuses a mismatch.
 */
final case class Manifest(
    header: Manifest.Header,
    classes: Seq[Manifest.ClassEntry],
    traits: Seq[Manifest.TraitEntry],
    exports: Seq[String],
    dynsigs: Seq[String]
) {
  lazy val payload: Array[Byte] = Manifest.encode(this)
  lazy val hash: String = readHash.getOrElse(Manifest.sha1Hex(payload))

  /** For a manifest read from a file: the hash the file has, which `read`
   *  verified, so that it is not encoded and hashed again.
   */
  @transient private var readHash: Option[String] = None

  /** For a manifest that is another one plus extra entries (a chain of
   *  extensions, `ManifestChain`): that one, whose lookups are extended instead
   *  of built again.
   */
  @transient private[live] var extending: Option[Manifest] = None

  lazy val classByName: Map[String, Manifest.ClassEntry] =
    extending match {
      case Some(base) =>
        base.classByName ++ classes.iterator
          .drop(base.classes.size)
          .map(c => c.name -> c)
      case None => classes.iterator.map(c => c.name -> c).toMap
    }
  lazy val traitByName: Map[String, Manifest.TraitEntry] =
    extending match {
      case Some(base) =>
        base.traitByName ++ traits.iterator
          .drop(base.traits.size)
          .map(t => t.name -> t)
      case None => traits.iterator.map(t => t.name -> t).toMap
    }
  lazy val exportSet: Set[String] =
    extending match {
      case Some(base) =>
        base.exportSet ++ exports.iterator.drop(base.exports.size)
      case None => exports.toSet
    }

  /** A host class's vtable signatures: its parent's slots, then its own. */
  def allSlots(className: String): Seq[String] =
    classByName.get(className).fold(Seq.empty[String]) { entry =>
      allSlots(entry.parent) ++ entry.vtable
    }

  /** Classes that a class of this manifest extends. */
  lazy val hasSubclasses: Set[String] =
    extending match {
      case Some(base) =>
        base.hasSubclasses ++
          classes.iterator
            .drop(base.classes.size)
            .map(_.parent)
            .filter(_.nonEmpty)
      case None => classes.iterator.map(_.parent).filter(_.nonEmpty).toSet
    }

  /** The classes of this manifest that extend each class directly. */
  lazy val subclassesByName: Map[String, Seq[Manifest.ClassEntry]] =
    extending match {
      case Some(base) =>
        classes.iterator
          .drop(base.classes.size)
          .filter(_.parent.nonEmpty)
          .foldLeft(base.subclassesByName) { (map, cls) =>
            map.updated(cls.parent, map.getOrElse(cls.parent, Nil) :+ cls)
          }
      case None => classes.filter(_.parent.nonEmpty).groupBy(_.parent)
    }

  /** Whether this manifest describes the class, module or trait: the host
   *  defines it or, in a chain's merged manifest, an earlier extension does.
   */
  def owns(name: nir.Global.Top): Boolean =
    classByName.contains(name.id) || traitByName.contains(name.id)

  def write(path: Path): Unit = {
    val out = new java.io.ByteArrayOutputStream()
    val w = new Manifest.Writer(out, _ => -1)
    w.int(Manifest.Magic)
    w.int(Manifest.FormatVersion)
    out.write(hash.getBytes(UTF_8))
    out.write(payload)
    Files.createDirectories(path.toAbsolutePath().getParent())
    Files.write(path, out.toByteArray())
  }

  /** A readable rendering, for debugging and size accounting. */
  def show: String = {
    val sb = new StringBuilder
    def line(s: String) = sb.append(s).append('\n')
    line(s"manifest $hash (format ${Manifest.FormatVersion})")
    line(s"header $header")
    classes.foreach { c =>
      line(
        s"class ${c.name} parent=${c.parent} flags=${c.flagNames.mkString(",")} size=${c.size}"
      )
      if (c.traits.nonEmpty) line(s"  traits ${c.traits.mkString(", ")}")
      c.fields.foreach(f => line(s"  field ${f.name} @${f.offset} ${f.ty}"))
      c.vtable.zipWithIndex.foreach { case (s, i) => line(s"  slot $i $s") }
      c.direct.foreach(d => line(s"  direct ${d.sig} -> ${d.target}"))
    }
    traits.foreach { t =>
      line(
        s"trait ${t.name}${if (t.open) " open" else ""} methods=${t.methods.size}"
      )
      t.methods.zipWithIndex.foreach { case (s, i) => line(s"  method $i $s") }
      t.direct.foreach(d => line(s"  direct ${d.sig} -> ${d.target}"))
    }
    line(s"exports ${exports.size}")
    exports.foreach(e => line(s"  $e"))
    dynsigs.foreach(d => line(s"dynsig $d"))
    sb.toString
  }
}

object Manifest {
  final val Magic = 0x464d4e53 // "SNMF", little-endian
  /** Version 2: the header has the link-time properties and service providers
   *  that the host's configuration sets.
   */
  final val FormatVersion = 2

  /** Version of the contract between extension code and the host's C runtime
   *  and loader: RTTI layout, descriptor layout, exported runtime symbols.
   */
  final val RuntimeAbiVersion = 1

  /** The host's settings that decide how generated code talks to the runtime,
   *  which an extension's code must share: threading, the collector, the kind
   *  of safepoint polls, lock words, C++ exceptions, and whether the host runs
   *  virtual threads, on whose carriers blocking calls compensate.
   */
  final case class Header(
      targetTriple: String,
      sizeOfPtr: Int,
      nirCompat: Int,
      nirRevision: Int,
      runtimeAbi: Int,
      multithreading: Boolean,
      gc: String,
      mode: String,
      usesLockWords: Boolean,
      yieldPointTraps: Boolean,
      cxxExceptions: Boolean,
      virtualThreads: Boolean,
      hostBinary: String,
      linktimeProperties: Seq[LinktimeProperty] = Nil,
      serviceProviders: Seq[ServiceEntry] = Nil
  )

  /** A link-time property that the host's configuration sets
   *  (`NativeConfig.linktimeProperties`), with its value as text and the kind
   *  of value: `Z` Boolean, `B` Byte, `C` Char, `S` Short, `I` Int, `J` Long,
   *  `F` Float, `D` Double, `T` String.
   */
  final case class LinktimeProperty(name: String, kind: String, text: String) {
    def value: Any = kind match {
      case "Z" => text.toBoolean
      case "B" => text.toByte
      case "C" => text.charAt(0)
      case "S" => text.toShort
      case "I" => text.toInt
      case "J" => text.toLong
      case "F" => text.toFloat
      case "D" => text.toDouble
      case _   => text
    }
  }

  object LinktimeProperty {

    /** The property, or none for a value that is not a primitive or a string,
     *  which an extension resolves from its own configuration.
     */
    def of(name: String, value: Any): Option[LinktimeProperty] = {
      def p(kind: String, text: Any) = Some(
        LinktimeProperty(name, kind, text.toString)
      )
      value match {
        case v: Boolean        => p("Z", v)
        case v: Byte           => p("B", v)
        case v: Char           => p("C", v)
        case v: Short          => p("S", v)
        case v: Int            => p("I", v)
        case v: Long           => p("J", v)
        case v: Float          => p("F", v)
        case v: Double         => p("D", v)
        case v: String         => p("T", v)
        case nir.Val.True      => p("Z", true)
        case nir.Val.False     => p("Z", false)
        case nir.Val.Byte(v)   => p("B", v)
        case nir.Val.Char(v)   => p("C", v)
        case nir.Val.Short(v)  => p("S", v)
        case nir.Val.Int(v)    => p("I", v)
        case nir.Val.Long(v)   => p("J", v)
        case nir.Val.Float(v)  => p("F", v)
        case nir.Val.Double(v) => p("D", v)
        case nir.Val.String(v) => p("T", v)
        case _                 => None
      }
    }
  }

  /** The providers of a service that the host's configuration loads
   *  (`NativeConfig.serviceProviders`).
   */
  final case class ServiceEntry(service: String, providers: Seq[String])

  object Flags {
    final val Open = 1
    final val Module = 2
    final val ConstantModule = 4
    final val Allocated = 8
    final val Abstract = 16
    val names = Seq(
      Open -> "open",
      Module -> "module",
      ConstantModule -> "constant-module",
      Allocated -> "allocated",
      Abstract -> "abstract"
    )
  }

  /** A field of a class's own declaration, in layout order. `name` is the
   *  mangled field global, `ty` the mangled type.
   */
  final case class FieldEntry(name: String, offset: Int, ty: String)

  /** A class or module. `fields` are the class's own, after its parent's.
   *  `vtable` holds the signatures of the slots it adds after its parent's, in
   *  slot order.
   */
  final case class ClassEntry(
      name: String,
      parent: String,
      traits: Seq[String],
      flags: Int,
      size: Int,
      fields: Seq[FieldEntry],
      vtable: Seq[String],
      direct: Seq[DirectEntry]
  ) {
    lazy val directBySig: Map[String, String] =
      direct.iterator.map(d => d.sig -> d.target).toMap
    lazy val fieldNames: Set[String] = fields.iterator.map(_.name).toSet
    def is(flag: Int): Boolean = (flags & flag) != 0
    def isOpen = is(Flags.Open)
    def isModule = is(Flags.Module)
    def isConstantModule = is(Flags.ConstantModule)
    def isAllocated = is(Flags.Allocated)
    def flagNames: Seq[String] =
      Flags.names.collect { case (f, n) if is(f) => n }
  }

  /** A virtual call the host devirtualized: calls of `sig` on receivers of the
   *  class or trait go straight to `target`, or nowhere when it is empty.
   */
  final case class DirectEntry(sig: String, target: String)

  /** A trait: its methods in itable order. */
  final case class TraitEntry(
      name: String,
      open: Boolean,
      methods: Seq[String],
      direct: Seq[DirectEntry]
  ) {
    lazy val directBySig: Map[String, String] =
      direct.iterator.map(d => d.sig -> d.target).toMap
  }

  // The magic, the format version and the payload's hash
  private final val HeaderSize = 48

  private val cache = mutable.Map.empty[Path, (Long, Array[Byte], Manifest)]

  /** Reads a manifest, verifying its hash; cached per path, size and header,
   *  whose hash identifies the contents: a host linked again within the file
   *  system's timestamp resolution has the same modification time.
   */
  def read(path: Path): Manifest = cache.synchronized {
    val abs = path.toAbsolutePath()
    if (!Files.exists(abs))
      throw new BuildException(s"Host manifest not found: $abs")
    cache.get(abs) match {
      case Some((size, header, m))
          if Files.size(abs) == size &&
            java.util.Arrays.equals(readHeader(abs), header) =>
        m
      case _ =>
        val bytes = Files.readAllBytes(abs)
        val m = decode(bytes, abs.toString)
        cache(abs) = (
          bytes.length.toLong,
          java.util.Arrays.copyOf(bytes, HeaderSize),
          m
        )
        m
    }
  }

  private def readHeader(path: Path): Array[Byte] = {
    val channel = FileChannel.open(path, StandardOpenOption.READ)
    try {
      val buffer = ByteBuffer.allocate(HeaderSize)
      while (buffer.hasRemaining() && channel.read(buffer) >= 0) ()
      buffer.array()
    } finally channel.close()
  }

  def decode(bytes: Array[Byte], source: String): Manifest = {
    def fail(msg: String) =
      throw new BuildException(s"Invalid host manifest $source: $msg")
    if (bytes.length < 48) fail("too short")
    val r = new Reader(bytes, 0)
    if (r.int() != Magic) fail("not a manifest")
    val version = r.int()
    if (version != FormatVersion)
      fail(s"format version $version, this linker reads $FormatVersion")
    val hash = new String(bytes, 8, 40, UTF_8)
    val payload = java.util.Arrays.copyOfRange(bytes, 48, bytes.length)
    if (sha1Hex(payload) != hash) fail("hash mismatch, the file is damaged")
    val p = new Reader(payload, 0)
    p.readStringTable()
    val header = Header(
      targetTriple = p.string(),
      sizeOfPtr = p.int(),
      nirCompat = p.int(),
      nirRevision = p.int(),
      runtimeAbi = p.int(),
      multithreading = p.bool(),
      gc = p.string(),
      mode = p.string(),
      usesLockWords = p.bool(),
      yieldPointTraps = p.bool(),
      cxxExceptions = p.bool(),
      virtualThreads = p.bool(),
      hostBinary = p.string(),
      linktimeProperties =
        p.seq(LinktimeProperty(p.string(), p.string(), p.string())),
      serviceProviders = p.seq(ServiceEntry(p.string(), p.seq(p.string())))
    )
    val classes = p.seq {
      ClassEntry(
        name = p.string(),
        parent = p.string(),
        traits = p.seq(p.string()),
        flags = p.int(),
        size = p.int(),
        fields = p.seq(FieldEntry(p.string(), p.int(), p.string())),
        vtable = p.seq(p.string()),
        direct = p.seq(DirectEntry(p.string(), p.string()))
      )
    }
    val traits = p.seq {
      TraitEntry(
        name = p.string(),
        open = p.bool(),
        methods = p.seq(p.string()),
        direct = p.seq(DirectEntry(p.string(), p.string()))
      )
    }
    val exports = p.seq {
      val owner = p.string()
      owner + p.string()
    }
    val dynsigs = p.seq(p.string())
    if (p.pos != payload.length) fail("trailing bytes")
    val m = Manifest(header, classes, traits, exports, dynsigs)
    m.readHash = Some(hash)
    m
  }

  private def encode(m: Manifest): Array[Byte] = {
    // Strings once, in a table, and then as indexes: signatures and names
    // repeat across vtables, itables and dispatch tables
    val strings = mutable.LinkedHashMap.empty[String, Int]
    def intern(str: String): Unit =
      if (!strings.contains(str)) strings(str) = strings.size
    val body = new java.io.ByteArrayOutputStream()
    encodeBody(m, new Writer(body, str => { intern(str); strings(str) }))
    val out = new java.io.ByteArrayOutputStream()
    val w = new Writer(out, _ => -1)
    w.int(strings.size)
    strings.keysIterator.foreach(w.rawString)
    out.write(body.toByteArray())
    out.toByteArray()
  }

  private def encodeBody(m: Manifest, w: Writer): Unit = {
    val h = m.header
    w.string(h.targetTriple)
    w.int(h.sizeOfPtr)
    w.int(h.nirCompat)
    w.int(h.nirRevision)
    w.int(h.runtimeAbi)
    w.bool(h.multithreading)
    w.string(h.gc)
    w.string(h.mode)
    w.bool(h.usesLockWords)
    w.bool(h.yieldPointTraps)
    w.bool(h.cxxExceptions)
    w.bool(h.virtualThreads)
    w.string(h.hostBinary)
    w.seq(h.linktimeProperties) { lp =>
      w.string(lp.name); w.string(lp.kind); w.string(lp.text)
    }
    w.seq(h.serviceProviders) { s =>
      w.string(s.service); w.seq(s.providers)(w.string)
    }
    w.seq(m.classes) { c =>
      w.string(c.name)
      w.string(c.parent)
      w.seq(c.traits)(w.string)
      w.int(c.flags)
      w.int(c.size)
      w.seq(c.fields) { f =>
        w.string(f.name); w.int(f.offset); w.string(f.ty)
      }
      w.seq(c.vtable)(w.string)
      w.seq(c.direct) { d => w.string(d.sig); w.string(d.target) }
    }
    w.seq(m.traits) { t =>
      w.string(t.name)
      w.bool(t.open)
      w.seq(t.methods)(w.string)
      w.seq(t.direct) { d => w.string(d.sig); w.string(d.target) }
    }
    // A member's owner and signature apart: signatures repeat across owners
    w.seq(m.exports) { e =>
      val split = memberSplit(e)
      w.string(e.substring(0, split))
      w.string(e.substring(split))
    }
    w.seq(m.dynsigs)(w.string)
  }

  /** Where the signature starts in a mangled member, `M<length><owner><sig>`.
   */
  private def memberSplit(mangled: String): Int =
    if (!mangled.startsWith("M")) 0
    else {
      var i = 1
      while (i < mangled.length && mangled.charAt(i).isDigit) i += 1
      // No digits, or a length too long for an Int, counts as 0
      val length =
        try mangled.substring(1, i).toInt
        catch { case _: NumberFormatException => 0 }
      math.min(i + length, mangled.length)
    }

  def sha1Hex(bytes: Array[Byte]): String = {
    val sha = new Sha1
    sha.update(bytes)
    sha.digest().map(b => f"${b & 0xff}%02x").mkString
  }

  private final class Writer(
      out: java.io.ByteArrayOutputStream,
      index: String => Int
  ) {
    def int(v: Int): Unit = {
      out.write(v & 0xff)
      out.write((v >>> 8) & 0xff)
      out.write((v >>> 16) & 0xff)
      out.write((v >>> 24) & 0xff)
    }
    def bool(v: Boolean): Unit = out.write(if (v) 1 else 0)
    def string(s: String): Unit = int(index(s))
    def rawString(s: String): Unit = {
      val bytes = s.getBytes(UTF_8)
      int(bytes.length)
      out.write(bytes)
    }
    def seq[T](xs: Seq[T])(f: T => Unit): Unit = {
      int(xs.size)
      xs.foreach(f)
    }
  }

  private final class Reader(bytes: Array[Byte], var pos: Int) {
    def int(): Int = {
      val v = (bytes(pos) & 0xff) | ((bytes(pos + 1) & 0xff) << 8) |
        ((bytes(pos + 2) & 0xff) << 16) | ((bytes(pos + 3) & 0xff) << 24)
      pos += 4
      v
    }
    def bool(): Boolean = {
      val v = bytes(pos) != 0
      pos += 1
      v
    }
    private var table: Array[String] = Array.empty
    def readStringTable(): Unit = {
      val n = int()
      table = new Array[String](n)
      var i = 0
      while (i < n) { table(i) = rawString(); i += 1 }
    }
    def string(): String = table(int())
    def rawString(): String = {
      val n = int()
      val s = new String(bytes, pos, n, UTF_8)
      pos += n
      s
    }
    def seq[T](f: => T): Seq[T] = {
      val n = int()
      val b = Vector.newBuilder[T]
      var i = 0
      while (i < n) { b += f; i += 1 }
      b.result()
    }
  }
}
