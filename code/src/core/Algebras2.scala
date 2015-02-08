/**
 * *****************************************************************************
 * Copyright (c) 2012-2013
 * - Bruno C.d.S. Oliveira (oliveira@comp.nus.edu.sg)
 * - Tijs van der Storm (storm@cwi.nl)
 * - Alex Loh (alexloh@cs.utexas.edu)
 * - William R. Cook (wcook@cs.utexas.edu)
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 * ******************************************************************************
 */

package core

object Algebras2 {
  import scala.reflect._

  trait Lifter[A, B] {
    def lift(x : A, y : B) : A with B
  }

  class MkLifter[A, B](f : (A, B) => A with B) extends Lifter[A, B] {
    def lift(x : A, y : B) : A with B = f(x, y)
  }

  trait Algebra[F[_, _]] {
    // Basic combinators
    def merge[A, B, S <: A with B](mix : Lifter[A, B], a1 : F[S, A], a2 : F[S, B]) : F[S, A with B]
    def empty[S] : F[S, Any]

    //
    def close[S <: A with B, A, B](alg : F[S, A with B]) : F[A with B, A with B] = alg.asInstanceOf[F[A with B, A with B]]
  }

  def createInstance[A](ih : java.lang.reflect.InvocationHandler)(implicit m : ClassTag[A]) : A = {
    java.lang.reflect.Proxy.newProxyInstance(m.runtimeClass.getClassLoader, Array(m.runtimeClass), ih).asInstanceOf[A]
  }

  def delegate[A, B, S <: A with B](x : A, y : B)(implicit m : ClassTag[S]) : S = createInstance[S](new java.lang.reflect.InvocationHandler() {
    def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
      try {
        method.invoke(x, args : _*)
      } catch {
        case e : IllegalArgumentException => method.invoke(y, args : _*)
      }
    }
  })

  trait AlgebraDefault[F[_, _]] {
    def merge[A, B, S <: A with B](mix : Lifter[A, B], a1 : F[S, A], a2 : F[S, B])(implicit m : ClassTag[F[S, A with B]]) : F[S, A with B] =
      createInstance[F[S, A with B]](new java.lang.reflect.InvocationHandler() {
        def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
          val a = method.invoke(a1, args : _*)
          val b = method.invoke(a2, args : _*)
          mix.lift(a.asInstanceOf[A], b.asInstanceOf[B]).asInstanceOf[Object]
        }
      })

    def combine[A, B, S <: A with B](alg1 : F[S, A], alg2 : F[S, B])(implicit m0 : ClassTag[S],
                                                                     m1 : ClassTag[F[S, A with B]]) : F[S, A with B] = {
      merge[A, B, S](new MkLifter[A, B](delegate[A, B, S] _), alg1, alg2)
    }

    def empty[S](implicit m : ClassTag[F[S, Any]]) : F[S, Any] =
      createInstance[F[S, Any]](new java.lang.reflect.InvocationHandler() {
        def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) = new Object()
      })
  }

  object Test {
    import Exp._

    object ExpComb extends AlgebraDefault[ExpAlg]

    /*
    def test = {
      import ExpComb._
      val o = exp(merge[IEval, IPrint](LiftEP,ExpEval,ExpPrint))
      println("Eval: " + o.eval() + "\nPrint:" + o.print())
    }
    */
  }
}

object Exp {
  import Algebras2.Lifter

  trait ExpAlg[In, Out] {
    def Lit(x : Int) : Out
    def Add(e1 : In, e2 : In) : Out
  }

  object ExpComb extends Algebras2.AlgebraDefault[ExpAlg]
  import ExpComb._

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

  def invExpAlg[In] : ExpAlg[In, InvExp[In]] = new InvExpAlg[In] {}

  trait IEval {
    def eval() : Int
  }

  trait PatternExpAlg[In <: InvExp[In], Out] extends ExpAlg[In, Out] { //Invertible observations for any constructor! 
    // Adding observations 
    //def fromLit(e : In) : Option[Int] = e.fromLit 
    //def fromAdd(e : In) : Option[(In,In)] = e.fromAdd 

    // Adding Observations with sugar! 
    object Lit { def unapply(e : In) : Option[Int] = e.fromLit }
    object Add { def unapply(e : In) : Option[(In, In)] = e.fromAdd }
  }

  trait EvalExpAlg[In <: IEval] extends ExpAlg[In, IEval] {
    def Lit(x : Int) = new IEval { def eval = x }

    def Add(e1 : In, e2 : In) = new IEval {
      def eval = e1.eval + e2.eval
    }
  }

  trait EvalExpAlg2[In <: InvExp[In] with IEval] extends PatternExpAlg[In, IEval] {
    def Lit(x : Int) = new IEval { def eval = x }

    def Add(e1 : In, e2 : In) = new IEval {
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

  def ExpEval[In <: IEval] : ExpAlg[In, IEval] = new EvalExpAlg[In] {}

  def ExpEval2[In <: InvExp[In] with IEval] : ExpAlg[In, IEval] = new EvalExpAlg2[In] {}

  trait IPrint {
    def print() : String
  }

  trait PrintExpAlg[In <: IPrint] extends ExpAlg[In, IPrint] {
    def Lit(x : Int) = new IPrint { def print = x.toString }

    def Add(e1 : In, e2 : In) = new IPrint {
      def print = e1.print() + " + " + e2.print()
    }
  }

  def ExpPrint[In <: IPrint] : ExpAlg[In, IPrint] = new PrintExpAlg[In] {}

  object LiftEP extends Lifter[IEval, IPrint] {
    def lift(x : IEval, y : IPrint) = new IEval with IPrint {
      def print() = y.print()
      def eval() = x.eval()
    }
  }

  object evalExpAlg extends EvalExpAlg[EvalInvExp]

  trait EvalInvExp extends InvExp[EvalInvExp] with IEval

  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))

  trait IEvalPrint extends IEval with IPrint

  object lifter extends Algebras2.Lifter[IEval, IPrint] {
    def lift(x : IEval, y : IPrint) : IEval with IPrint = new IEval with IPrint {
      def print = y.print
      def eval = x.eval
    }
  }

  def lifter2[In <: InvExp[In] with IEval] = new Algebras2.Lifter[InvExp[In], IEval] {
    def lift(a : InvExp[In], b : IEval) : InvExp[In] with IEval = new InvExp[In] with IEval {
      val fromLit = a.fromLit
      val fromAdd = a.fromAdd
      def eval = b.eval
    }

    //def conv[In](InvExp[In] with IEval) : In
  }

  /*
   def merge[A,B,S <: A with B](mix : Lifter[A,B], a1 : F[S,A], a2 : F[S,B]) (implicit m : ClassTag[F[S,A with B]]) : F[S,A with B] = 
      createInstance[F[S, A with B]](new java.lang.reflect.InvocationHandler() {
      def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
        val a = method.invoke(a1,args : _*) 
        val b = method.invoke(a2,args : _*)
        mix.lift(a.asInstanceOf[A],b.asInstanceOf[B]).asInstanceOf[Object]
      }
    })
   */

  // Editing Start

  trait ExpMerge[A, B, SA <: A, SB <: B, S <: SA with SB] extends ExpAlg[S, A with B] {
    val lifter : Lifter[A, B]
    val alg1 : ExpAlg[SA, A]
    val alg2 : ExpAlg[SB, B]

    def Lit(x : Int) : A with B =
      lifter.lift(alg1.Lit(x), alg2.Lit(x))

    def Add(e1 : S, e2 : S) : A with B =
      lifter.lift(alg1.Add(e1, e2), alg2.Add(e1, e2))
  }
  trait combEvalPrint extends ExpMerge[IEval, IPrint, IEval, IPrint, IEval with IPrint] {
    val lifter = LiftEP
    val alg1 = ExpEval[IEval]
    val alg2 = ExpPrint[IPrint]
  }
  object combEvalPrint extends combEvalPrint
  /*
  trait ExpMerge2[A, B, S <: A with B] extends ExpAlg[S, A with B] {
    type SA <: A
    type SB <: B 
    type S <: SA with SB
    
    val lifter : Lifter[A,B]
    val alg1  : ExpAlg[SA, A]
    val alg2  : ExpAlg[SB, B]
  
    def Lit(x : Int) : A with B =
      lifter.lift(alg1.Lit(x),alg2.Lit(x))
    
    def Add(e1 : S, e2 : S) : A with B =
      lifter.lift(alg1.Add(e1, e2),alg2.Add(e1, e2))
  }
  * 
  */
  /*
  trait combEvalPrint2 extends ExpMerge2[IEval, IPrint, IEval with IPrint] {
    val lifter = LiftEP
    val alg1 = ExpEval[IEval] 
    val alg2 = ExpPrint[IPrint] 
  }
  object combEvalPrint2 extends combEvalPrint2
  */
  /*
  trait combInvEval extends ExpMerge[InvExp[EvalInvExp], IEval, InvExp[EvalInvExp] with IEval] {
    val alg1 = invExpAlg[EvalInvExp] // : ExpAlg[InvExp[EvalInvExp], InvExp[EvalInvExp]] 
    val alg2 = ExpEval2 //: ExpAlg[InvExp[EvalInvExp] with IEval, IEval]
    val lifter = lifter2  
  }
  * 
  */

  //Editing End
}

object Main2 extends App {
  import Exp._
  import ExpComb._

  def mymerge[In <: InvExp[In] with IEval] : ExpAlg[In, InvExp[In] with IEval] =
    merge[InvExp[In], IEval, In](lifter2, invExpAlg, ExpEval2)

  def composition = mymerge[EvalInvExp].asInstanceOf[ExpAlg[InvExp[EvalInvExp] with IEval, InvExp[EvalInvExp] with IEval]]

  override def main(args : Array[String]) {
    val o = exp(composition)
    //val o = exp(merge[InvExp[In],IEval, IEval with IPrint](lifter,ExpEval,ExpPrint))
    //val o = exp(merge[IEval, IPrint, IEval with IPrint](lifter,ExpEval,ExpPrint))
    //val o = exp(combine[IEval, IPrint, IEvalPrint](ExpEval,ExpPrint.asInstanceOf[ExpAlg[IEvalPrint,IPrint]]))

    println("Eval: " + o.eval())

    val e = exp(combEvalPrint)
    println(">>> combine Eval Print")
    println("Eval: " + e.eval + "\nPrint: " + e.print)
  }
}