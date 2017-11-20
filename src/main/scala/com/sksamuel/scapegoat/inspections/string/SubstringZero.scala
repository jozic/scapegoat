package com.sksamuel.scapegoat.inspections.string

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }

/** @author Stephen Samuel */
object SubstringZero extends Inspection("String.substring(0)", Levels.Info) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private val Substring = TermName("substring")
      private val StringType = typeOf[String]

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(lhs, Substring), List(Literal(Constant(0)))) if lhs.tpe <:< StringType =>
            context.warn(tree.pos, self,
              "Use of String.substring(0) will always return the same string: " + tree.toString().take(100))
          case _ => continue(tree)
        }
      }
    }
  }
}
