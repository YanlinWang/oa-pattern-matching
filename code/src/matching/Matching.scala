package matching

import core.Algebras.AlgebraDefault

object Matching {

  trait ExpAlg[In, Out] {
    def Lit(x : Int) : Out
    def Add(e1 : In, e2 : In) : Out
  }

  trait InvExp[Exp] {
    val fromLit : Option[Int]
    val fromAdd : Option[(Exp, Exp)]
  }

  trait InvExpAlg[In] extends ExpAlg[In, InvExp[In]] {
    def Lit(x : Int) = new InvExp[In] {
      val fromLit = Some(x)
      val fromAdd = None
    }
    def Add(e1 : In, e2 : In) = new InvExp[In] {
      val fromLit = None
      val fromAdd = Some(e1, e2)
    }
  }

  object invExpAlg extends InvExpAlg[EvalInvExp]

  trait Eval {
    def eval : Int
  }
  
  /*
  trait Eval[In] {
    def eval : Int
  }*/

  trait PatternExpAlg[In <: InvExp[In], Out] extends ExpAlg[In, Out] { //Invertible observations for any constructor! 
    // Adding observations 
    //def fromLit(e : In) : Option[Int] = e.fromLit 
    //def fromAdd(e : In) : Option[(In,In)] = e.fromAdd 

    // Adding Observations with sugar! 
    object Lit { def unapply(e : In) : Option[Int] = e.fromLit }
    object Add { def unapply(e : In) : Option[(In, In)] = e.fromAdd }
  }

  trait EvalExpAlg[In <: InvExp[In] with Eval] extends PatternExpAlg[In, Eval] {
    def Lit(x : Int) = new Eval { def eval = x }

    def Add(e1 : In, e2 : In) = new Eval {
      //      def eval = e1.fromLit match { 
      //        case Some(n) => System.out.println("Been here"); n + e2.eval 
      //        case None => e1.eval + e2.eval 
      //      }  
      def eval = e1 match {
        case Lit(n) => System.out.println("Been here"); n + e2.eval
        case Add(_, _) => System.out.println("Been there"); e1.eval + e2.eval
        case _ => e1.eval + e2.eval
      }
    }
  }

  object evalExpAlg extends EvalExpAlg[EvalInvExp]

  // Structural equality with Object Algebras 
  trait Eq[In] {
    def equal(o : In) : Boolean
  }

  trait EqExpAlg[In <: InvExp[In] with Eq[In]] extends PatternExpAlg[In, Eq[In]] {
    def Lit(x : Int) = new Eq[In] {
      def equal(o : In) = o match {
        case Lit(y) => x == y // calls fromLit 
        case _      => false
      }
    }

    def Add(e1 : In, e2 : In) = new Eq[In] {
      def equal(o : In) = o match {
        case Add(e3, e4) => e1.equal(e3) && e2.equal(e4) // calls fromAdd 
        case _           => false
      }
    }
  }

  trait EvalInvExp extends Eval with InvExp[EvalInvExp] // type-level Fixpoint: seems necessary for allowing binary method-like stuff. No real need for F-bounds 

  def mix(a : Eval, b : InvExp[EvalInvExp]) : EvalInvExp = new EvalInvExp {
    val fromLit = b.fromLit
    val fromAdd = b.fromAdd
    def eval = a.eval
  }

  // Mixing the 2 algebras 
  trait EvalWithInAlg extends ExpAlg[EvalInvExp, EvalInvExp] {
    def Lit(x : Int) = mix(evalExpAlg.Lit(x), invExpAlg.Lit(x))
    def Add(e1 : EvalInvExp, e2 : EvalInvExp) = mix(evalExpAlg.Add(e1,
      e2), invExpAlg.Add(e1, e2))
  }
  
  //trait Merge[F[_],G[_],In] extends ExpAlg[EvalInvExp, EvalInvExp] {
  //  def Lit(x : Int) = mix(evalExpAlg.Lit(x), invExpAlg.Lit(x))
  //  def Add(e1 : EvalInvExp, e2 : EvalInvExp) = mix(evalExpAlg.Add(e1,
  //    e2), invExpAlg.Add(e1, e2))
  //}

  object EvalWithInAlg extends EvalWithInAlg

  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))

  def test = System.out.println(exp(EvalWithInAlg).eval) // combine(evalAlg,invAlg) 

  
}
//  object Test {
//    import Exp._
//    object ExpComb extends Algebra[ExpAlg]
//  
//    def test = {
//      import ExpComb._
//      val o = exp(merge[IEval, IPrint](LiftEP,ExpEval,ExpPrint))
//      println("Eval: " + o.eval() + "\nPrint:" + o.print())
//    }
//  }
object Test {
  import Matching._
  object ExpComb extends AlgebraDefault[ExpAlg]
  
  def test = {
    import ExpComb._
  }
}

object Main {
  def main(args : Array[String]) {
    val t = Matching.test
  }
}