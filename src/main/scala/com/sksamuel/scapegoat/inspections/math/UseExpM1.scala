package com.sksamuel.scapegoat.inspections.math

import com.sksamuel.scapegoat._

/** @author Matic Potočnik */
object UseExpM1 extends Inspection("Use expm1", Levels.Info) {

  def inspector(context: InspectionContext): Inspector = new Inspector(context) {
    override def postTyperTraverser = Some apply new context.Traverser {

      import context.global._

      def isMathPackage(pack: String) =
        (pack == "scala.math.`package`"
          || pack == "java.this.lang.Math"
          || pack == "java.this.lang.StrictMath")

      override def inspect(tree: Tree): Unit = {
        tree match {
          case Apply(Select(Apply(Select(pack, TermName("exp")), List(number)), nme.SUB), List(Literal(Constant(1)))) =>
            val math = pack.toString().stripSuffix(".`package`").substring(pack.toString().lastIndexOf('.'))
            context.warn(tree.pos, self,
              s"$math.expm1(x) is clearer and more performant than $math.exp(x) - 1")

          case _ => continue(tree)
        }
      }
    }
  }
}
