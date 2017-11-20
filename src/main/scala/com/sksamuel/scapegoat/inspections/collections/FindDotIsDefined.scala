package com.sksamuel.scapegoat.inspections.collections

import com.sksamuel.scapegoat._

/** @author Stephen Samuel */
object FindDotIsDefined extends Inspection("use exists() not find().isDefined()", Levels.Info) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Select(Apply(Select(_, TermName("find")), _), TermName("isDefined")) =>
            context.warn(tree.pos, self,
              ".find(x => Bool).isDefined can be replaced with exists(x => Bool): " + tree.toString().take(500))
          case _ => continue(tree)
        }
      }
    }
  }
}