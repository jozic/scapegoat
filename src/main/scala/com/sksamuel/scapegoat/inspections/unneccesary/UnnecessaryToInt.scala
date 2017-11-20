package com.sksamuel.scapegoat.inspections.unneccesary

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }

/** @author Stephen Samuel */
object UnnecessaryToInt extends Inspection("Unnecessary toInt", Levels.Warning) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._
      import definitions._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Select(lhs, TermName("toInt")) if lhs.tpe <:< IntClass.tpe =>
            context.warn(tree.pos, self,
              "Unnecessary invocation of toInt on instance of Int " + tree.toString().take(200))
          case _ =>
        }
        continue(tree)
      }
    }
  }
}
