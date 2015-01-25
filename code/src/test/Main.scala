package test

import core.Algebras._
import core.Exp._

object Exp {
  
  /*
   * StackAlg -> ExpAlg
   * SelfStackAlg -> SelfExpAlg
   */
  trait SelfExpAlg[S <: E, E] extends ExpAlg[S,Open[S, E]] with SelfFAlg[ExpAlg,S,E]  
  
  /*
   * OpenStackAlg -> OpenExpAlg
   */
  type OpenExpAlg[S <: E, E] = (=> ExpAlg[S,S]) => ExpAlg[S,Open[S, E]]
  
  /* EF : Eval Feature
   * SF -> EF
   * stack() -> Lit() and Add()
   * SelfStackAlg -> SelfEvalAlg
   */
  trait EF[S <: IEval] extends SelfExpAlg[S, IEval] { 
    def Lit(x:Int) = self => new IEval() { def eval = x }
    def Add(e1:S,e2:S) = self => new IEval() { def eval = e1.eval + e2.eval }
  }
  def ef[S <: IEval] : OpenExpAlg[S,IEval] = 
    s => new EF[S] { lazy val fself = s }
    
  /* PF : Print Feature */  
  trait PF[S <: IPrint] extends SelfExpAlg[S, IPrint] { 
    def Lit(x:Int) = self => new IPrint() { def print = x.toString }
    def Add(e1:S,e2:S) = self => new IPrint() { def print = "(" + e1.print + "+" + e2.print + ")" }
  }
  def pf[S <: IPrint] : OpenExpAlg[S,IPrint] = 
    s => new PF[S] { lazy val fself = s }

  object LiftEP extends Lifter[IEval, IPrint, IEval with IPrint] {
    def lift(e : IEval, p : IPrint) = self => new IEval with IPrint {
      def eval = e.eval
      def print = p.print
    }
  }
}

object ExpComb extends AlgebraDefault[ExpAlg] 

object Main extends App {  
  import Exp._
  import ExpComb._
  
  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))
  
  override def main(args: Array[String]) {
    println(">>> Eval Feature")
    testEval()
    println(">>> Print Feature")
    testPrint()
    
    // TODO : error test, combine eval and print feature  
    println(">>> Eval + Print")
    testEP()
  }
  
  def testEval() = {
    val o = exp[IEval](fclose(ef))
    println(o.eval)
  }
  def testPrint() = {
    val o = exp[IPrint](fclose(pf))
    println(o.print)
  }
  
  // combine version : with ClassCastException
  def testEP_combine() = {
    val o = exp[IEval with IPrint](fclose(combine[IEval,IPrint,IEval with IPrint](ef, pf))) // exception line
    println("Eval: " + o.eval + "\nPrint: " + o.print)
  }
  // merge version : no run-time errors, but have to define the lifter LiftEP
  def testEP() = {
    val o = exp[IEval with IPrint](fclose(merge[IEval,IPrint,IEval with IPrint](LiftEP, ef, pf)))
    println("Eval: " + o.eval + "\nPrint: " + o.print)
  }
}