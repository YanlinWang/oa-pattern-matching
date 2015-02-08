package matching

import core.Algebras.AlgebraDefault

object Matching2 {
  /*
  trait ExpAlg[In, Out] {
    def Lit(x : Int) : Out
    def Add(e1 : In, e2 : In) : Out
  }

  trait InvExp {
    type Exp
    val fromLit : Option[Int]
    val fromAdd : Option[(Exp, Exp)]
  }

  trait InvExpAlg[In] extends ExpAlg[In, InvExp] {
    def Lit(x : Int) = new InvExp {
      type Exp = In
      val fromLit = Some(x)
      val fromAdd = None
    }
    def Add(e1 : In, e2 : In) = new InvExp {
      type Exp = In
      val fromLit = None
      val fromAdd = Some(e1, e2)
    }
  }

  object invExpAlg extends InvExpAlg[EvalInvExp]

  def invExpAlg2[In] : ExpAlg[In, InvExp] = new InvExpAlg[In] {}

  trait Eval {
    def eval : Int
  }

  trait Print {
    def print : String
  }

  /*
  trait Eval[In] {
    def eval : Int
  }*/

  trait PatternExpAlg[In <: InvExp { type Exp = In }, Out] extends ExpAlg[In, Out] { //Invertible observations for any constructor! 
    // Adding observations 
    //def fromLit(e : In) : Option[Int] = e.fromLit 
    //def fromAdd(e : In) : Option[(In,In)] = e.fromAdd 

    // Adding Observations with sugar! 
    object Lit { def unapply(e : In) : Option[Int] = e.fromLit }
    object Add { def unapply(e : In) : Option[(In, In)] = e.fromAdd }
  }

  trait EvalExpAlg[In <: Eval with InvExp { type Exp = In }] extends PatternExpAlg[In, Eval] {
    def Lit(x : Int) = new Eval { def eval = x }

    def Add(e1 : In, e2 : In) = new Eval {
      //      def eval = e1.fromLit match { 
      //        case Some(n) => System.out.println("Been here"); n + e2.eval 
      //        case None => e1.eval + e2.eval 
      //      }  
      def eval = e1 match {
        case Lit(n) =>
          System.out.println("Been here"); n + e2.eval
        case Add(_, _) =>
          System.out.println("Been there"); e1.eval + e2.eval
        case _ => e1.eval + e2.eval
      }
    }
  }


  
  object evalExpAlg extends EvalExpAlg[EvalInvExp]

  def evalExpAlg2[In <: Eval with InvExp { type Exp = In }] : ExpAlg[In, Eval] = new EvalExpAlg[In] {}

  trait EvalInvExp extends Eval with InvExp //{ type Exp = EvalInvExp } // type-level Fixpoint: seems necessary for allowing binary method-like stuff. No real need for F-bounds 
  /*
  def mix(a : Eval, b : InvExp) : EvalInvExp = new EvalInvExp {
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
*/
  trait ExpMerge[S, A, B] extends ExpAlg[S, A with B] {
    val lift : A => B => A with B
    val alg1 : ExpAlg[S, A]
    val alg2 : ExpAlg[S, B]

    def Lit(x : Int) : A with B =
      lift(alg1.Lit(x))(alg2.Lit(x))

    def Add(e1 : S, e2 : S) : A with B =
      lift(alg1.Add(e1, e2))(alg2.Add(e1, e2))
  }

  def merge[S, A, B](mix : A => B => A with B, a1 : ExpAlg[S, A], a2 : ExpAlg[S, B]) : ExpAlg[S, A with B] = new ExpMerge[S, A, B] {
    val lift = mix
    val alg1 = a1
    val alg2 = a2
  }
  def mix2 : Eval => InvExp => Eval with InvExp = a => b => new Eval with InvExp {
    type Exp = b.Exp
    val fromLit = b.fromLit
    val fromAdd = b.fromAdd
    def eval = a.eval
  }

  // crazy coercions
  def fun2(o : Eval with InvExp) : EvalInvExp = new EvalInvExp() {
    type Exp = o.Exp
    val fromLit = o.fromLit
    val fromAdd = o.fromAdd
    def eval = o.eval
  }

  def fun(alg : ExpAlg[EvalInvExp, Eval with InvExp]) : ExpAlg[EvalInvExp, EvalInvExp] =
    new ExpAlg[EvalInvExp, EvalInvExp] {
      def Lit(x : Int) : EvalInvExp = fun2(alg.Lit(x))
      def Add(e1 : EvalInvExp, e2 : EvalInvExp) : EvalInvExp = fun2(alg.Add(e1, e2))
    }

  /*
  def dual(alg : ExpAlg[EvalInvExp, Eval with InvExp[EvalInvExp]]) : ExpAlg[Eval with InvExp[EvalInvExp], Eval with InvExp[EvalInvExp]] =
    new ExpAlg[Eval with InvExp[EvalInvExp], Eval with InvExp[EvalInvExp]] {
      def Lit(x : Int) : Eval with InvExp[EvalInvExp] = alg.Lit(x)
      def Add(e1 : Eval with InvExp[EvalInvExp], e2 : Eval with InvExp[EvalInvExp]) : Eval with InvExp[EvalInvExp] = alg.Add(fun2(e1), fun2(e2))
    }

  def dual2(alg : ExpAlg[EvalInvExp, Eval with InvExp[EvalInvExp]]) : ExpAlg[Eval with InvExp[EvalInvExp], Eval with InvExp[EvalInvExp]] =
    alg.asInstanceOf[ExpAlg[Eval with InvExp[EvalInvExp], Eval with InvExp[EvalInvExp]]]

*/
  def test5 = {
      /*
      def pre[In <: InvExp[In] with Eval] : ExpAlg[In, InvExp[In] with Eval] = merge[In, Eval, InvExp[In]](mix2, evalExpAlg2, invExpAlg2)

      def pre2 : ExpAlg[EvalInvExp, Eval with InvExp[EvalInvExp]] = pre[EvalInvExp] // In = EvalInvExp

      def o = exp(dual2(pre2))
    println("Eval:" + o.eval)

      def o2 = exp(fun(pre2))
    println("Eval:" + o2.eval + "\nfromLit:" + o2.fromLit + "\nfromAdd:" + o2.fromAdd)
    * 
    */

      def pre[In <: InvExp with Eval] = merge[In, Eval, InvExp](mix2, evalExpAlg2, invExpAlg2)

      def pre2 : ExpAlg[EvalInvExp, Eval with InvExp] = pre[EvalInvExp]

      def o = exp(fun(pre2))

    println(o.eval)

  }

  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))

  //trait Merge[F[_],G[_],In] extends ExpAlg[EvalInvExp, EvalInvExp] {
  //  def Lit(x : Int) = mix(evalExpAlg.Lit(x), invExpAlg.Lit(x))
  //  def Add(e1 : EvalInvExp, e2 : EvalInvExp) = mix(evalExpAlg.Add(e1,
  //    e2), invExpAlg.Add(e1, e2))
  //}

  /*
  object EvalWithInAlg extends EvalWithInAlg


  def test = System.out.println(exp(EvalWithInAlg).eval) // combine(evalAlg,invAlg) 
  * 
  */
  * 
  */

}