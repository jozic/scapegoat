package com.sksamuel.scapegoat.inspections.nulls

import com.sksamuel.scapegoat._

/** @author Stephen Samuel */
object NullAssignment extends Inspection("Null assignment", Levels.Warning) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      def containsNull(trees: List[Tree]) = trees exists {
        case Literal(Constant(null)) => true
        case _                       => false
      }

      override def inspect(tree: Tree): Unit = {
        tree match {
          case ValDef(_, _, _, Literal(Constant(null))) => warn(tree)
          case Apply(Select(_, name), List(Literal(Constant(null)))) =>
            if (name.endsWith("_$eq"))
              warn(tree)
          case Assign(_, Literal(Constant(null))) => warn(tree)
          case DefDef(mods, _, _, _, _, _) if mods.hasFlag(Flag.SYNTHETIC) =>
          case _ => continue(tree)
        }
      }

      private def warn(tree: Tree) {
        context.warn(tree.pos, self, "Null assignment on line " + tree.pos.line)
      }
    }
  }
}