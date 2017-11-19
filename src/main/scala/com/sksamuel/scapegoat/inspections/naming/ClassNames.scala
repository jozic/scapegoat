package com.sksamuel.scapegoat.inspections.naming

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }

/** @author Stephen Samuel */
object ClassNames extends Inspection("Class name not recommended", Levels.Info) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      private val regex = "^[A-Z][A-Za-z0-9]*$"

      override def inspect(tree: Tree): Unit = {
        tree match {
          case ClassDef(_, name, _, _) if name.toString.contains("$anon") =>
          case ClassDef(mods, name, _, _) if !mods.isSynthetic && !name.toString.matches(regex) =>
            context.warn(tree.pos, self ,
              s"Class names should begin with uppercase letter (bad = $name)")
          case _ => continue(tree)
        }
      }
    }
  }
}
