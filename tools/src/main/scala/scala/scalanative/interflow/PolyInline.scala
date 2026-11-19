package scala.scalanative
package interflow

import scala.collection.mutable

import scalanative.linker._

private[interflow] trait PolyInline { self: Interflow =>

  object PolyInlined {
    def unapply(input: (nir.Val, Seq[nir.Val], nir.Type.Function))(implicit
        state: State,
        srcPosition: nir.SourcePosition,
        scopeId: nir.ScopeId
    ): Option[nir.Val] = {
      val (value, eargs, callty) = input
      value match {
        case DelayedRef(op: nir.Op.Method) =>
          mode match {
            case build.Mode.Debug =>
              None

            case _: build.Mode.Release =>
              val allTargets = polyTargets(op)
              // A phantom stands for extension classes: every inlined case
              // gets a class check, and other classes dispatch virtually
              val open = analysis.phantoms.nonEmpty &&
                allTargets.exists {
                  case (cls, _) => analysis.isPhantom(cls.name)
                }
              val targets =
                if (!open) allTargets
                else
                  allTargets.filterNot {
                    case (cls, _) => analysis.isPhantom(cls.name)
                  }
              val classes = targets.map(_._1)
              val classesCount = classes.size
              val impls = targets.map(_._2).distinct
              val implsCount = impls.size

              val shallPolyInline =
                if (mode == build.Mode.ReleaseFast || mode == build.Mode.ReleaseSize) {
                  classesCount <= 8 && (implsCount == 2 || (open && implsCount == 1))
                } else {
                  classesCount <= 16 && implsCount >= (if (open) 1
                                                       else
                                                         2) && implsCount <= 4
                }

              if (shallPolyInline) {
                in(s"polyinline ${op.obj.ty.show} ${op.sig.show}") {
                  Some(
                    polyInline(
                      op,
                      eargs.toIndexedSeq,
                      targets,
                      classes,
                      impls,
                      if (open) Some(callty) else None
                    )
                  )
                }
              } else {
                log(
                  s"no reason to polyinline ${op.obj.ty.show} ${op.sig.show}}"
                )
                None
              }
          }
        case _ => None
      }
    }
  }

  private def polyTargets(
      op: nir.Op.Method
  )(implicit state: State): IndexedSeq[(Class, nir.Global.Member)] = {
    val nir.Op.Method(obj, sig) = op

    val objty = obj match {
      case InstanceRef(ty) =>
        ty
      case _ =>
        obj.ty
    }

    val res = objty match {
      case ExactClassRef(cls, _) =>
        cls.resolve(sig).map(g => (cls, g)).toIndexedSeq
      case ClassRef(cls) if !sig.isVirtual =>
        cls.resolve(sig).map(g => (cls, g)).toIndexedSeq
      // A receiver of a host type in an extension link: the host's classes
      // are unknown
      case ScopeRef(scope) if isHostScope(scope) =>
        IndexedSeq.empty
      case ScopeRef(scope) =>
        val targets = mutable.UnrolledBuffer.empty[(Class, nir.Global.Member)]
        scope.implementors.foreach { cls =>
          if (cls.allocated) {
            cls.resolve(sig).foreach { g => targets += ((cls, g)) }
          }
        }
        targets.toIndexedSeq
      case _ =>
        IndexedSeq.empty
    }

    // the only case when result won't be empty or one element seq is reading from `scop.implementors`
    // scop.implementors are prestorted by the same way => I don't need sort here.
    res
  }

  private def polyInline(
      op: nir.Op.Method,
      args: IndexedSeq[nir.Val],
      targets: IndexedSeq[(Class, nir.Global.Member)],
      classes: IndexedSeq[Class],
      impls: IndexedSeq[nir.Global.Member],
      virtualFallback: Option[nir.Type.Function]
  )(implicit
      state: State,
      analysis: ReachabilityAnalysis.Result,
      srcPosition: nir.SourcePosition,
      scopeIdId: nir.ScopeId
  ): nir.Val = {
    import state.{emit, fresh, materialize}

    val obj = materialize(op.obj)
    val margs = args.map(materialize(_))

    // With a virtual fallback the last class is checked too
    val checked =
      if (virtualFallback.isDefined) targets.size else targets.size - 1
    val checkLabels = IndexedSeq.fill(checked)(fresh())
    val callLabels = IndexedSeq.fill(impls.size)(fresh())
    val callLabelIndex =
      (0 until targets.size).map(i => impls.indexOf(targets(i)._2)).toIndexedSeq
    val mergeLabel = fresh()
    val fallbackLabel = virtualFallback.map(_ => fresh())

    val objcls = fallbackLabel match {
      case None =>
        state.getClassCache.getOrElseUpdate(
          obj, {
            val meth = emit.method(obj, nir.Rt.GetClassSig, nir.Next.None)
            val methty = nir.Type.Function(Seq(nir.Rt.Object), nir.Rt.Class)
            emit.call(methty, meth, Seq(obj), nir.Next.None)
          }
        )
      case Some(fallback) =>
        // With a fallback the checks read the object's RTTI pointer in line;
        // a null receiver takes the virtual call, which throws
        val loadLabel = fresh()
        val nonNull =
          emit.comp(
            nir.Comp.Ine,
            nir.Rt.Object,
            obj,
            nir.Val.Null,
            nir.Next.None
          )
        emit.branch(nonNull, nir.Next(loadLabel), nir.Next(fallback))
        emit.label(loadLabel)
        val raw = emit.conv(nir.Conv.Bitcast, nir.Type.Ptr, obj, nir.Next.None)
        emit.load(nir.Type.Ptr, raw, nir.Next.None)
    }
    val clsty = if (fallbackLabel.isDefined) nir.Type.Ptr else nir.Rt.Class

    for (idx <- 0.until(checkLabels.length)) {
      val checkLabel = checkLabels(idx)
      if (idx > 0) {
        emit.label(checkLabel)
      }
      val cls = classes(idx)
      val isCls = emit.comp(
        nir.Comp.Ieq,
        clsty,
        objcls,
        nir.Val.Global(cls.name, nir.Rt.Class),
        nir.Next.None
      )
      if (idx < checked - 1) {
        emit.branch(
          isCls,
          nir.Next(callLabels(callLabelIndex(idx))),
          nir.Next(checkLabels(idx + 1))
        )
      } else {
        emit.branch(
          isCls,
          nir.Next(callLabels(callLabelIndex(idx))),
          fallbackLabel.fold(nir.Next(callLabels(callLabelIndex(idx + 1))))(
            nir.Next(_)
          )
        )
      }
    }

    val rettys = mutable.UnrolledBuffer.empty[nir.Type]

    // Classes that no case checks: a virtual call
    for {
      label <- fallbackLabel
      callty <- virtualFallback
    } {
      emit.label(label, Seq.empty)
      val nir.Type.Function(argtys, retty) = callty
      rettys += retty
      val meth = emit.method(obj, op.sig, nir.Next.None)
      val cargs = margs.zip(argtys).map {
        case (value, argty) =>
          if (Sub.is(value.ty, argty)) value
          else emit.conv(nir.Conv.Bitcast, argty, value, nir.Next.None)
      }
      val res = emit.call(callty, meth, cargs, nir.Next.None)
      emit.jump(nir.Next.Label(mergeLabel, Seq(res)))
    }

    for (i <- 0.until(callLabels.length)) {
      val callLabel = callLabels(i)
      val m = impls(i)

      emit.label(callLabel, Seq.empty)
      val ty = originalFunctionType(m)
      val nir.Type.Function(argtys, retty) = ty
      rettys += retty

      val cargs = margs.zip(argtys).map {
        case (value, argty) =>
          if (Sub.is(value.ty, argty)) value
          else emit.conv(nir.Conv.Bitcast, argty, value, nir.Next.None)
      }
      // Keep each target's virtual heap and cached expressions branch-local.
      // Arguments are materialized above; materializing the result before the
      // join means no branch-local allocation needs to escape into State.
      // Follow the direct-call path so an unvisited target is optimized before
      // shallInline examines it, including argument-type specialization.
      val (target, targetType) = visitDuplicate(m, cargs.map(_.ty))
        .map(defn => (defn.name, defn.ty))
        .getOrElse {
          visitRoot(m)
          (m, ty)
        }
      val callState = state.fullClone(callLabel)
      callState.fresh = fresh
      callState.emit = new nir.InstructionBuilder()(fresh)
      val value =
        if (shallInline(target, cargs)(callState, analysis))
          `inline`(target, cargs)(callState, analysis, scopeIdId)
        else
          callState.emit.call(
            targetType,
            nir.Val.Global(target, nir.Type.Ptr),
            cargs,
            nir.Next.None
          )
      val res = callState.materialize(value)
      emit ++= callState.emit
      if (preserveDebugInfo) {
        state.localNames.addMissing(callState.localNames)
        state.virtualNames.addMissing(callState.virtualNames)
      }
      emit.jump(nir.Next.Label(mergeLabel, Seq(res)))
    }

    val result = nir.Val.Local(fresh(), Sub.lub(rettys.toSeq, Some(op.resty)))
    emit.label(mergeLabel, Seq(result))

    result
  }

}
