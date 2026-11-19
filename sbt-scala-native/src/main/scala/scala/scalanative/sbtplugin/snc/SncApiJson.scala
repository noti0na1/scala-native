package scala.scalanative.sbtplugin.snc

import xsbti.api._

/** Decodes the JSON form of Zinc's API trees (`xsbti.api`) that `snc`'s `api`
 *  events carry (events protocol 1.1, described in the README of the companion
 *  Scala 3 fork). Nodes are arrays whose first element is a tag:
 *
 *  {{{
 *  ["CL", name, access, modifiers, annotations, definitionType, selfType,
 *         structure, savedAnnotations, childrenOfSealedClass, topLevel,
 *         typeParameters]
 *  }}}
 *
 *  Modifiers are a bit set in `Modifiers`' constructor order (bit 0 abstract,
 *  ... bit 7 super accessor). Zinc hashes the decoded trees itself, so
 *  invalidation is decided by the same code as for the JVM compiler.
 */
private[scalanative] object SncApiJson {

  def classLike(json: Any): ClassLike = {
    val a = node(json, "CL")
    ClassLike.of(
      str(a(1)),
      access(a(2)),
      modifiers(a(3)),
      annotations(a(4)),
      definitionType(a(5)),
      SafeLazy.strict(tpe(a(6))),
      SafeLazy.strict(structure(a(7))),
      seq(a(8)).map(str).toArray,
      seq(a(9)).map(tpe).toArray,
      bool(a(10)),
      typeParameters(a(11))
    )
  }

  private def node(json: Any, tag: String): Vector[Any] = {
    val a = seq(json)
    if (a.isEmpty || a.head != tag)
      throw new IllegalArgumentException(s"expected $tag, got $json")
    a
  }

  private def seq(json: Any): Vector[Any] = json match {
    case v: Vector[_] => v.asInstanceOf[Vector[Any]]
    case _ => throw new IllegalArgumentException(s"expected an array: $json")
  }

  private def str(json: Any): String = json match {
    case s: String => s
    case _ => throw new IllegalArgumentException(s"expected a string: $json")
  }

  private def bool(json: Any): Boolean = json == true

  private def int(json: Any): Int = json match {
    case n: BigDecimal => n.toInt
    case _ => throw new IllegalArgumentException(s"expected a number: $json")
  }

  private def access(json: Any): Access = json match {
    case "pub"                                        => Public.of()
    case v: Vector[_] if v.headOption.contains("pro") =>
      Protected.of(qualifier(v(1)))
    case v: Vector[_] if v.headOption.contains("pri") =>
      Private.of(qualifier(v(1)))
    case _ => throw new IllegalArgumentException(s"expected an access: $json")
  }

  private def qualifier(json: Any): Qualifier = json match {
    case "u"                                        => Unqualified.of()
    case "t"                                        => ThisQualifier.of()
    case v: Vector[_] if v.headOption.contains("i") => IdQualifier.of(str(v(1)))
    case _ => throw new IllegalArgumentException(s"expected a qualifier: $json")
  }

  private def modifiers(json: Any): Modifiers = {
    val bits = int(json)
    def bit(i: Int) = (bits & (1 << i)) != 0
    new Modifiers(
      bit(0),
      bit(1),
      bit(2),
      bit(3),
      bit(4),
      bit(5),
      bit(6),
      bit(7)
    )
  }

  private def annotations(json: Any): Array[Annotation] =
    seq(json).map { a =>
      val v = seq(a)
      Annotation.of(
        tpe(v(0)),
        seq(v(1)).map { arg =>
          val p = seq(arg)
          AnnotationArgument.of(str(p(0)), str(p(1)))
        }.toArray
      )
    }.toArray

  private def definitionType(json: Any): DefinitionType = json match {
    case "trait"   => DefinitionType.Trait
    case "class"   => DefinitionType.ClassDef
    case "module"  => DefinitionType.Module
    case "package" => DefinitionType.PackageModule
    case _ => throw new IllegalArgumentException(s"definition type: $json")
  }

  private def variance(json: Any): Variance = json match {
    case "co"     => Variance.Covariant
    case "contra" => Variance.Contravariant
    case _        => Variance.Invariant
  }

  private def typeParameters(json: Any): Array[TypeParameter] =
    seq(json).map { t =>
      val v = seq(t)
      TypeParameter.of(
        str(v(0)),
        annotations(v(1)),
        typeParameters(v(2)),
        variance(v(3)),
        tpe(v(4)),
        tpe(v(5))
      )
    }.toArray

  private def structure(json: Any): Structure = {
    val a = node(json, "S")
    Structure.of(
      SafeLazy.strict(seq(a(1)).map(tpe).toArray),
      SafeLazy.strict(seq(a(2)).map(definition).toArray),
      SafeLazy.strict(seq(a(3)).map(definition).toArray)
    )
  }

  private def path(json: Any): Path =
    Path.of(seq(json).map { c =>
      val v = seq(c)
      (v.head match {
        case "i"   => Id.of(str(v(1)))
        case "s"   => Super.of(path(v(1)))
        case "t"   => This.of()
        case other =>
          throw new IllegalArgumentException(s"path component: $other")
      }): PathComponent
    }.toArray)

  private def tpe(json: Any): Type = {
    val a = seq(json)
    a.head match {
      case "P"    => Projection.of(tpe(a(1)), str(a(2)))
      case "Z"    => Parameterized.of(tpe(a(1)), seq(a(2)).map(tpe).toArray)
      case "R"    => ParameterRef.of(str(a(1)))
      case "S"    => structure(json)
      case "Poly" => Polymorphic.of(tpe(a(1)), typeParameters(a(2)))
      case "C"    => Constant.of(tpe(a(1)), str(a(2)))
      case "X"    => Existential.of(tpe(a(1)), typeParameters(a(2)))
      case "Y"    => Singleton.of(path(a(1)))
      case "A"    => Annotated.of(tpe(a(1)), annotations(a(2)))
      case "E"    => EmptyType.of()
      case other  => throw new IllegalArgumentException(s"type: $other")
    }
  }

  private def parameterModifier(json: Any): ParameterModifier = json match {
    case "rep"    => ParameterModifier.Repeated
    case "byname" => ParameterModifier.ByName
    case _        => ParameterModifier.Plain
  }

  private def definition(json: Any): ClassDefinition = {
    val a = seq(json)
    def name = str(a(1))
    def acc = access(a(2))
    def mods = modifiers(a(3))
    def annots = annotations(a(4))
    a.head match {
      case "Def" =>
        Def.of(
          name,
          acc,
          mods,
          annots,
          typeParameters(a(5)),
          seq(a(6)).map { l =>
            val list = seq(l)
            ParameterList.of(
              seq(list(0)).map { p =>
                val v = seq(p)
                MethodParameter.of(
                  str(v(0)),
                  tpe(v(1)),
                  bool(v(2)),
                  parameterModifier(v(3))
                )
              }.toArray,
              bool(list(1))
            )
          }.toArray,
          tpe(a(7))
        )
      case "Val" => Val.of(name, acc, mods, annots, tpe(a(5)))
      case "Var" => Var.of(name, acc, mods, annots, tpe(a(5)))
      case "TA"  =>
        TypeAlias.of(name, acc, mods, annots, typeParameters(a(5)), tpe(a(6)))
      case "TD" =>
        TypeDeclaration.of(
          name,
          acc,
          mods,
          annots,
          typeParameters(a(5)),
          tpe(a(6)),
          tpe(a(7))
        )
      case "CLD" =>
        ClassLikeDef.of(
          name,
          acc,
          mods,
          annots,
          typeParameters(a(5)),
          definitionType(a(6))
        )
      case other => throw new IllegalArgumentException(s"definition: $other")
    }
  }
}
