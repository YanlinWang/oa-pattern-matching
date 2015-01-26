package test

import core.Algebras._
import core.Exp._

object Exp2 {
  object ExpComb extends AlgebraDefault[ExpAlg] 

  trait SelfExpAlg[S <: E, E] extends ExpAlg[S,Open[S, E]] with SelfFAlg[ExpAlg,S,E]  

  type OpenExpAlg[S <: E, E] = (=> ExpAlg[S,S]) => ExpAlg[S,Open[S, E]]

  trait EF[S <: IEval] extends SelfExpAlg[S, IEval] { 
    def Lit(x:Int) = self => new IEval() { def eval = x }
    def Add(e1:S,e2:S) = self => new IEval() { def eval = e1.eval + e2.eval }
  }
  def ef[S <: IEval] : OpenExpAlg[S,IEval] = 
    s => new EF[S] { lazy val fself = s }

  trait InvF[S <: InvExp[S]] extends SelfExpAlg[S, InvExp[S]] {
    def Lit(x:Int) = self => new InvExp[S] {
      val fromLit = Some(x)
      val fromAdd = None
    }
    def Add(e1:S, e2:S) = self => new InvExp[S] {
      val fromLit = None
      val fromAdd = Some(e1,e2)
    }
  }
    
  def invf[S <: InvExp[S]] : OpenExpAlg[S, InvExp[S]] =   
    s => new InvF[S] { lazy val fself = s}

}



object Main2 extends App {  
  import Exp2._
  import ExpComb._

  // TODO: What I want to express. But this is syntax error
  //def exp[E](alg : ExpAlg[S <: E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))
  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))
  
  override def main(args: Array[String]) {
    println(">>> Eval Feature")
    testEval()
    println(">>> Inv Feature")
    testInv()
  }
  
  def testEval() = {
    val o = exp[IEval](fclose(ef))
    println(o.eval)
  }
  
  def testInv() = {
    // TODO
    //val o = exp[InvExp[EvalInvExp]] ( fclose( invf[InvExp[EvalInvExp]].asInstanceOf[OpenExpAlg[InvExp[EvalInvExp],InvExp[EvalInvExp]]] ) ) //type error
    //val o = exp[InvExp[IEval]](fclose(invf[InvExp[IEval]]))           //type error
  }
}