package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat.{Inspection, InspectionContext, Inspector, Levels}

/** @author Stephen Samuel */
object LonelySealedTrait extends Inspection("Lonely sealed trait", Levels.Error) {

  override def inspector(context: InspectionContext): Inspector = new Inspector(context) {

    import context.global._

    override def postTyperTraverser = Some apply new context.Traverser {

      override def inspect(tree: Tree): Unit = {
        tree match {
          case cdef @ ClassDef(mods, _, _, _) if mods.isSealed =>
            if (cdef.tpe.typeSymbol.knownDirectSubclasses.isEmpty)
              context.warn(cdef.pos, self,
                snippet = s"Sealed trait ${cdef.name} has no implementing classes")
          case _ =>
        }
        continue(tree)
      }
    }
  }
}
