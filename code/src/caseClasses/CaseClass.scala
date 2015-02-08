package caseClasses

object CaseClass {
  trait Exp
  case class Lit(x : Int) extends Exp
  case class Add(e1 : Exp, e2 : Exp) extends Exp

  def eval(e : Exp) : Int = e match {
    case Lit(x)               => x
    case Add(Add(e1, e2), e3) => eval(e1) + eval(e2) + eval(e3)
  }

  case class Mul(e1 : Exp, e2 : Exp) extends Exp

  //
}