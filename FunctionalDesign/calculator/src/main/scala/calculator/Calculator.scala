package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    for {
      (name, expr) <- namedExpressions
      sigVal = Signal(eval(expr(), namedExpressions))
    } yield (name, sigVal)
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def ev(expr: Expr) = eval(expr, references)

    expr match {
      case Literal(v) => v
      case Plus(a, b) => ev(a) + ev(b)
      case Minus(a, b) => ev(a) - ev(b)
      case Times(a, b) => ev(a) * ev(b)
      case Divide (a, b) => ev(a) / ev(b)
      case Ref(name) =>
        if (!references.contains(name)) Double.NaN
        else eval(references(name)(), references - name)
      case _ => throw new UnsupportedOperationException
    }
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
