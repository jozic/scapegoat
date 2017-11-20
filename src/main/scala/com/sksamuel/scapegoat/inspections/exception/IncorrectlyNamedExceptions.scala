package com.sksamuel.scapegoat.inspections.exception

import com.sksamuel.scapegoat._

/**
 * @author Stephen Samuel
 *
 *         Inspired by http://findbugs.sourceforge.net/bugDescriptions.html#NM_CLASS_NOT_EXCEPTION
 */
object IncorrectlyNamedExceptions extends Inspection("Incorrectly Named Exceptions", Levels.Error) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      override def inspect(tree: Tree): Unit = {
        tree match {
          case cdef @ ClassDef(mods, name, _, impl) =>
            val isNamedException = name.toString.endsWith("Exception")
            val isAnon = scala.util.Try {
              cdef.symbol.isAnonymousClass
            } getOrElse false

            val extendsException = impl.tpe <:< typeOf[Exception]
            val selfTypeIsException = impl match {
              case Template(_, self, _) =>
                self.tpt.tpe <:< typeOf[Exception]
              case _ => false
            }

            // A class or trait is an Exception for our purposes if it either
            // inherits from exception or it is a trait which declares its
            // self-type to be Exception
            val isException = extendsException || selfTypeIsException

            (isNamedException, isAnon, isException) match {
              case (true, _, false) =>
                context.warn(tree.pos, self,
                  "Class named exception does not derive from Exception" + tree.toString().take(500))
              case (false, false, true) =>
                context.warn(tree.pos, self,
                  "Class derived from Exception is not named *Exception" + tree.toString().take(500))
              case _ =>
            }
          case _ =>
        }
        continue(tree)
      }
    }
  }
}