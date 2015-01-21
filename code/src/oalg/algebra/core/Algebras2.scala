/*******************************************************************************
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
********************************************************************************/

package oalg.algebra.core

object Algebras2 {
  import scala.reflect._
  
  trait Lifter[A,B] {
    def lift(x : A, y : B) : A with B
  }

  class MkLifter[A,B](f : (A,B) => A with B) extends Lifter[A,B] {
    def lift(x : A, y : B) : A with B = f(x,y)
  }
  
  trait Algebra[F[_,_]] {
    // Basic combinators
    def merge[A,B,S <: A with B](mix : Lifter[A,B], a1 : F[S,A], a2 : F[S,B]) : F[S,A with B]   
    def empty[S] : F[S,Any] 
  
  }
  
  def createInstance[A](ih : java.lang.reflect.InvocationHandler)(implicit m : ClassTag[A]) : A = {
      java.lang.reflect.Proxy.newProxyInstance(m.runtimeClass.getClassLoader, Array(m.runtimeClass),ih).asInstanceOf[A]
  }
 
  def delegate[A,B, S <: A with B](x : A, y : B)(implicit m : ClassTag[S]) : S = createInstance[S](new java.lang.reflect.InvocationHandler() {
      def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
	      try {
	        method.invoke(x, args : _*)
	      } catch {
	        case e : IllegalArgumentException => method.invoke(y, args : _*)
	      }
	    }
    })
     
  trait AlgebraDefault[F[_,_]] {  
    def merge[A,B,S <: A with B](mix : Lifter[A,B], a1 : F[S,A], a2 : F[S,B]) (implicit m : ClassTag[F[S,A with B]]) : F[S,A with B] = 
      createInstance[F[S, A with B]](new java.lang.reflect.InvocationHandler() {
	    def invoke(proxy : Object, method : java.lang.reflect.Method, args : Array[Object]) : Object = {
	      val a = method.invoke(a1,args : _*) 
	      val b = method.invoke(a2,args : _*)
	      mix.lift(a.asInstanceOf[A],b.asInstanceOf[B]).asInstanceOf[Object]
	    }
    })
    
    def combine[A, B, S <: A with B](alg1 : F[S, A], alg2 : F[S, B])
        (implicit 
            m0 : ClassTag[S],
            m1 : ClassTag[F[S, A with B]]) : F[S, A with B] = {
      merge[A,B,S](new MkLifter[A, B](delegate[A,B,S] _), alg1, alg2)
    }
    
    def empty[S](implicit m : ClassTag[F[S,Any]]) : F[S,Any] = 
      createInstance[F[S,Any]](new java.lang.reflect.InvocationHandler() {
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

  def invExpAlg[In] : ExpAlg[In,InvExp[In]] = new InvExpAlg[In] {}
   
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
      def eval =  e1.eval + e2.eval
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
        case Lit(n) => System.out.println("Been here"); n + e2.eval
        case Add(_, _) => System.out.println("Been there"); e1.eval + e2.eval
        case _ => e1.eval + e2.eval
      }
    }
  }
  
  def ExpEval[In <: IEval] : ExpAlg[In,IEval] = new EvalExpAlg[In] {}
  
  def ExpEval2[In <: InvExp[In] with IEval] : ExpAlg[In,IEval] = new EvalExpAlg2[In] {}
  
  trait IPrint {
    def print() : String
  }
  
  trait PrintExpAlg[In <: IPrint] extends ExpAlg[In, IPrint] {
    def Lit(x : Int) = new IPrint { def print = x.toString }

    def Add(e1 : In, e2 : In) = new IPrint {
      def print = e1.print() + " + " + e2.print()
    }
  }
  
  def ExpPrint[In <: IPrint] : ExpAlg[In,IPrint] = new PrintExpAlg[In] {}

  object evalExpAlg extends EvalExpAlg[EvalInvExp]
  
  trait EvalInvExp extends InvExp[EvalInvExp] with IEval
  
  def exp[E](alg : ExpAlg[E, E]) = alg.Add(alg.Lit(3), alg.Lit(4))
  
  trait IEvalPrint extends IEval with IPrint
  
  object lifter extends Algebras2.Lifter[IEval,IPrint] {
    def lift(x : IEval, y : IPrint) : IEval with IPrint = new IEval with IPrint {
      def print = y.print
      def eval = x.eval
    }
  }
  
  def lifter2[In <: InvExp[In] with IEval] = new Algebras2.Lifter[InvExp[In],IEval] {
    def lift(a : InvExp[In], b : IEval) : InvExp[In] with IEval = new InvExp[In] with IEval {
      val fromLit = a.fromLit
      val fromAdd = a.fromAdd
      def eval = b.eval
    }
    
    //def conv[In](InvExp[In] with IEval) : In
  }
  
  def test = {
   def mymerge[In <: InvExp[In] with IEval] : ExpAlg[In,InvExp[In] with IEval]= 
     merge[InvExp[In],IEval, In](lifter2,invExpAlg,ExpEval2)
    
   def composition = mymerge[EvalInvExp].asInstanceOf[ExpAlg[InvExp[EvalInvExp] with IEval,InvExp[EvalInvExp] with IEval]]
   val o = exp(composition)
   //val o = exp(merge[InvExp[In],IEval, IEval with IPrint](lifter,ExpEval,ExpPrint))
   //val o2 = exp(merge[IEval, IPrint, IEval with IPrint](lifter,ExpEval,ExpPrint))
   //val o = exp(combine[IEval, IPrint, IEvalPrint](ExpEval,ExpPrint.asInstanceOf[ExpAlg[IEvalPrint,IPrint]]))
   
   println("Eval: " + o.eval())
  }
  
  //def test = System.out.println(exp(EvalWithInAlg).eval) 
  
  /*
  trait ExpEval extends ExpAlg[IEval] {  
    def Lit(x : Int) : IEval = new IEval {
      def eval() : Int = x
    }
    
    def Add(e1 : IEval, e2 : IEval) : IEval = new IEval {
      def eval() : Int = e1.eval() + e2.eval() 
    }
  }
  
  object ExpEval extends ExpEval
  
  
  trait IPrint {
    def print() : String
  }
  
  trait ExpPrint extends ExpAlg[IPrint] {  
    def Lit(x : Int) : IPrint = new IPrint {
      def print() : String = x.toString()
    }
    
    def Add(e1 : IPrint, e2 : IPrint) : IPrint = new IPrint {
      def print() : String = e1.print() + " + " + e2.print() 
    }
  }
  
  object ExpPrint extends ExpPrint
  
  object OATesting {
    
    def exp[Exp](f : ExpAlg[Exp]) : Exp =
      f.Add(f.Lit(5), f.Add(f.Lit(6),f.Lit(6)))
      
    val test1 = {
      val o1 : IEval = exp(ExpEval)
      val o2 : IPrint = exp(ExpPrint)
      
      println("Eval: " + o1.eval() + "\nPrint: " + o2.print())
    }
    
  }
  
  
  // Combinators using pairs
  
  trait ExpMergePair[A,B] extends ExpAlg[(A,B)] {
    val alg1 : ExpAlg[A]
    val alg2 : ExpAlg[B]
    
    def Lit(x : Int) : (A,B) = 
      (alg1.Lit(x), alg2.Lit(x))
      
    def Add(e1 : (A,B), e2 : (A,B)) : (A,B) =
      (alg1.Add(e1._1,e2._1), alg2.Add(e1._2,e2._2))
  }
  
  object OACTesting {
     
    def exp[Exp](f : ExpAlg[Exp]) : Exp =
      f.Add(f.Lit(5), f.Add(f.Lit(6),f.Lit(6)))
  
    object ExpPrintEval extends ExpMergePair[IPrint,IEval] {
      val alg1 = ExpPrint
      val alg2 = ExpEval
    } 
      
    val test2 = {
      val o : (IPrint,IEval) = exp(ExpPrintEval)
      println("Eval: " + o._2.eval() + "\nPrint: " + o._1.print())
    }
  }
  */
}