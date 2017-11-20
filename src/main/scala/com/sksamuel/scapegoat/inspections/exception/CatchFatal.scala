package com.sksamuel.scapegoat.inspections.exception

import com.sksamuel.scapegoat.{ Inspection, InspectionContext, Inspector, Levels }

import scala.util.control.ControlThrowable

/** @author Marconi Lanna */
object CatchFatal extends Inspection("Catch fatal exception", Levels.Warning) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      def isFatal(tpe: context.global.Type) = {
        tpe =:= typeOf[VirtualMachineError] ||
          tpe =:= typeOf[ThreadDeath] ||
          tpe =:= typeOf[InterruptedException] ||
          tpe =:= typeOf[LinkageError] ||
          tpe =:= typeOf[ControlThrowable]
      }

      def catchesFatal(cases: List[CaseDef]) = {
        cases.exists {
          // matches t : FatalException
          case CaseDef(Bind(_, Typed(_, tpt)), _, _) if isFatal(tpt.tpe) => true
          // matches _ : FatalException
          case CaseDef(Typed(_, tpt), _, _) if isFatal(tpt.tpe) => true
          case _ => false
        }
      }

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Try(_, cases, _) if catchesFatal(cases) =>
            context.warn(tree.pos, self,
              "Did you intend to catch a fatal exception? Consider using scala.util.control.NonFatal: " +
                tree.toString().take(300))
          case _ => continue(tree)
        }
      }
    }
  }
}
