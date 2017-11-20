package com.sksamuel.scapegoat.inspections

import com.sksamuel.scapegoat._

/** @author Stephen Samuel */
object EmptyCaseClass extends Inspection("Empty case class", Levels.Info,
  "Empty case class can be rewritten as a case object") {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      def accessors(trees: List[Tree]): List[ValDef] = {
        trees.collect {
          case v: ValDef => v
        }.filter(_.mods.isCaseAccessor)
      }

      override def inspect(tree: Tree): Unit = {
        tree match {
          // body should have constructor only, and with synthetic methods it has 10 in total
          case ClassDef(mods, _, List(), Template(_, _, body)) if mods.isCase && accessors(body).isEmpty =>
            context.warn(tree.pos, self)
          case _ => continue(tree)
        }
      }
    }
  }
}

