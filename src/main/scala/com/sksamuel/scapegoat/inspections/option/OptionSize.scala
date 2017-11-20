package com.sksamuel.scapegoat.inspections.option

import com.sksamuel.scapegoat._

/** @author Stephen Samuel */
object OptionSize extends Inspection("Prefer Option.isDefined instead of Option.size", Levels.Error) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Select(Apply(option2Iterable, List(opt)), TermName("size")) ⇒
            if (option2Iterable.symbol.fullName == "scala.Option.option2Iterable")
              context.warn(tree.pos, self, tree.toString().take(500))
          case _ ⇒ continue(tree)
        }
      }
    }
  }
}