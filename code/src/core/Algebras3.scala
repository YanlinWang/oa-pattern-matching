package core
import Algebras2._
object Exp3 {
  trait ExpAlg[-In, +Out] {
    def Lit(x:Int):Out
    def Add(e1:In, e2:In):Out
  }
  trait IEval { def eval : Int}
  trait EvalExpAlg[-In <: IEval] extends ExpAlg[In, IEval] {
    def Lit(x:Int) = new IEval { def eval = x}
    def Add(e1:In, e2:In) = new IEval { def eval = e1.eval + e2.eval}
  }
  def ExpEval[In <: IEval] = new EvalExpAlg[In] {}
  
  trait IPrint {def print : String}
  trait PrintExpAlg[-In <: IPrint] extends ExpAlg[In, IPrint] {
    def Lit(x:Int) = new IPrint { def print = x.toString}
    def Add(e1:In, e2:In) = new IPrint { def print = e1.print + "+" + e2.print}
  }
  /*
  trait MergeExp[+A, +B, -InA,-InB, -S <:InA with InB] extends ExpAlg[S, A with B] {
    val lifter : Lifter[A,B]
    val alg1 : ExpAlg[InA, A]
    val alg2 : ExpAlg[InB, B]
    def Lit(x:Int) = lifter.lift(alg1.Lit(x), alg2.Lit(x))
    def Add(e1:S, e2:S) = lifter.lift(alg1.Add(e1, e2), alg2.Add(e1,e2));
  }
  * 
  */
  
  //object EvalPrintAlg extends MergeExp[IEval,IPrint,IEval,IPrint,IEval with IPrint]
  
  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))
  def test = {
    val o = exp(ExpEval[IEval])
    println(o.eval)
  }
}