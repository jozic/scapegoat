package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }

/** @author Stephen Samuel */
object MaxParameters extends Inspection("Max parameters", Levels.Info) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private def count(vparamss: List[List[ValDef]]): Int =
        vparamss.foldLeft(0)((a, b) => a + b.size)

      private def countExceeds(vparamss: List[List[ValDef]], limit: Int) =
        count(vparamss) > limit

      override def inspect(tree: Tree): Unit = {
        tree match {
          case DefDef(_, name, _, _, _, _) if name == nme.CONSTRUCTOR =>
          case DefDef(mods, _, _, _, _, _) if mods.isSynthetic        =>
          case DefDef(_, name, _, vparamss, _, _) if countExceeds(vparamss, 10) =>
            context.warn(tree.pos, self,
              s"Method $name has ${count(vparamss)} parameters. Consider refactoring to a containing instance.")
          case _ => continue(tree)
        }
      }
    }
  }
}
