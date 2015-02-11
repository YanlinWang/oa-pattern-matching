package matching
import core.Algebras2.AlgebraDefault
import core.Algebras2.Lifter

object RegionExample {
  object RegionComb extends AlgebraDefault[RegionAlg]
  import RegionComb._

  trait RegionAlg[In, Out] {
    def univ(x : Unit) : Out
    def circle(radius : Double) : Out
    def union(reg1 : In, reg2 : In) : Out
  }

  trait Eval { def eval : (Double, Double) => Boolean }

  trait EvalRegionAlg[In <: Eval] extends RegionAlg[In, Eval] {
    def univ(x : Unit) : Eval = new Eval { def eval = (_, _) => true }
    def circle(radius : Double) = new Eval { def eval = (x, y) => x * x + y * y <= radius * radius }
    def union(reg1 : In, reg2 : In) = new Eval { def eval = (x, y) => reg1.eval(x, y) || reg2.eval(x, y) }
  }
  object evalRegionAlg extends EvalRegionAlg[Eval]

  trait EvalRegionAlg2[In <: InvRegion[In] with Eval] extends PatternRegionAlg[In, Eval] {
    def univ(x : Unit) : Eval = new Eval { def eval = (_, _) => true }
    def circle(radius : Double) = new Eval { def eval = (x, y) => x * x + y * y <= radius * radius }
    def union(reg1 : In, reg2 : In) = new Eval {
      def eval = (x, y) => (reg1, reg2) match {
        case (univ(_ : Unit), _) => true
        case (_, univ(_ : Unit)) => true
        case _                   => reg1.eval(x, y) || reg2.eval(x, y)
      }
    }
  }
  def evalRegionAlg2[In <: InvRegion[In] with Eval] : PatternRegionAlg[In, Eval] = new EvalRegionAlg2[In] {}

  trait Transform[Exp] { def transform : Exp }

  trait EvalInv extends Eval with InvRegion[EvalInv]

  trait EvalInv2 extends InvRegion[EvalInv2] with Transform[Eval] with Eval

  trait EvalRegionAlg3[E, In <: InvRegion[In] with Transform[E]] extends PatternRegionAlg[In, Transform[E]] {
    val alg : PatternRegionAlg[In, E]

    def univ(x : Unit) : Transform[E] = new Transform[E] { def transform = alg.univ({}) }
    def circle(radius : Double) = new Transform[E] { def transform = alg.circle(radius) }
    def union(reg1 : In, reg2 : In) = new Transform[E] {
      def transform = (reg1, reg2) match {
        case (univ(_ : Unit), _) => alg.univ({})
        case (_, univ(_ : Unit)) => alg.univ({})
        case _                   => alg.union(reg1, reg2)
      }
    }
  }

  trait InvRegion[R] {
    val fromUniv : Option[Unit]
    val fromCircle : Option[Double]
    val fromUnion : Option[(R, R)]
  }

  trait InvRegionAlg[In] extends RegionAlg[In, InvRegion[In]] {
    def univ(x : Unit) = new InvRegion[In] {
      val fromUniv = Some()
      val fromCircle = None
      val fromUnion = None
    }
    def circle(radius : Double) = new InvRegion[In] {
      val fromUniv = None
      val fromCircle = Some(radius)
      val fromUnion = None
    }
    def union(reg1 : In, reg2 : In) = new InvRegion[In] {
      val fromUniv = None
      val fromCircle = None
      val fromUnion = Some(reg1, reg2)
    }
  }
  def invRegionAlg[In] : RegionAlg[In, InvRegion[In]] = new InvRegionAlg[In] {}

  trait PatternRegionAlg[In <: InvRegion[In], Out] extends RegionAlg[In, Out] {
    object univ { def unapply(e : In) : Option[Unit] = e.fromUniv }
    object circle { def unapply(e : In) : Option[Double] = e.fromCircle }
    object union { def unapply(e : In) : Option[(In, In)] = e.fromUnion }
  }

  def test5() = { // use combine
      def evalAlg3 : PatternRegionAlg[EvalInv2, Transform[Eval]] = new EvalRegionAlg3[Eval, EvalInv2] {
        val alg : PatternRegionAlg[EvalInv2, Eval] = evalRegionAlg2[EvalInv2]
      }
      def evalInvAlg : RegionAlg[EvalInv2, Transform[Eval] with InvRegion[EvalInv2]] =
        combine[Transform[Eval], InvRegion[EvalInv2], EvalInv2](evalAlg3, invRegionAlg)
      def o = makeRegion(closeS[EvalInv2, Transform[Eval], InvRegion[EvalInv2]](evalInvAlg))

    println("test5 >>>")
    println("Is (0.5,0.5) inside it? " + o.transform.eval(0.5, 0.5))
  }

  object liftTransInv extends Lifter[Transform[Eval], InvRegion[EvalInv2]] {
    def lift(x : Transform[Eval], y : InvRegion[EvalInv2]) : Transform[Eval] with InvRegion[EvalInv2] =
      new Transform[Eval] with InvRegion[EvalInv2] {
        def transform : Eval = x.transform
        val fromUniv = y.fromUniv
        val fromCircle = y.fromCircle
        val fromUnion = y.fromUnion
      }
  }
  /*
  object liftTransInv_weird extends Lifter[Transform[Eval], InvRegion[EvalInv2]] with Eval {
    var eval_res : Eval = null
    def lift(x : Transform[Eval], y : InvRegion[EvalInv2]) : Transform[Eval] with InvRegion[EvalInv2] =
      new Transform[Eval] with InvRegion[EvalInv2] {
        def transform : Eval = x.transform
        val fromUniv = y.fromUniv
        val fromCircle = y.fromCircle
        val fromUnion = y.fromUnion
        eval_res = new Eval { def eval = transform.eval }
      }
    def eval = eval_res.eval
  }
  * 
  */

  def test6() = { // use merge
    println("test6 >>>")
    val evalAlg3 : PatternRegionAlg[EvalInv2, Transform[Eval]] =
      new EvalRegionAlg3[Eval, EvalInv2] {
        val alg : PatternRegionAlg[EvalInv2, Eval] = evalRegionAlg2[EvalInv2]
      }

    val evalInvAlg : RegionAlg[EvalInv2, Transform[Eval] with InvRegion[EvalInv2]] =
      merge[Transform[Eval], InvRegion[EvalInv2], EvalInv2](liftTransInv, evalAlg3, invRegionAlg)

    val pre : RegionAlg[Transform[Eval] with InvRegion[EvalInv2], Transform[Eval] with InvRegion[EvalInv2]] = closeS(evalInvAlg)

    //val o : Transform[Eval] with InvRegion[EvalInv2] = makeRegion(pre)   // error
    //    val o : Transform[Eval] with InvRegion[EvalInv2] = { import pre._; univ({}) } // correct
    //    val o : Transform[Eval] with InvRegion[EvalInv2] = { import pre._; circle(1.0) } // correct
    //    val o : Transform[Eval] with InvRegion[EvalInv2] = { import pre._; union(univ({}), circle(0.1)) } // correct
    //    val o : Transform[Eval] with InvRegion[EvalInv2] = { import pre._; union(circle(0.1), univ({})) } // correct
    val o : Transform[Eval] with InvRegion[EvalInv2] = { import pre._; union(circle(1.0), circle(1.0)) }

    /* In union(circle(1.0), circle(1.0)) ,
     * circle(1.0)  :  Transform[Eval] with InvRegion[EvalInv2]
     * circle(1.0) is passes into union(,) function as argument reg1 (which requires to support eval)
     * so it tries to cast 
     * This is the reason of runtime class casting error.
     */

    println("fromUniv: " + o.fromUniv)
    println("fromCircle: " + o.fromCircle)
    println("fromUnion: " + o.fromUnion)
    println("fromUnion: " + o.fromUnion.get._1.transform.eval(0.1, 0.1))

    val trans = o.transform
    val eva = trans.eval(0.5, 0.5)
    println("eval:" + eva)
  }

  def test2() = {
    println(">>> Testing EvalRegionAlg2")
      // method 1: merge by hand
      def pre[In <: InvRegion[In] with Eval] : RegionAlg[In, InvRegion[In] with Eval] =
        regionMerge[In, Eval, InvRegion[In]](mixEvalInv, evalRegionAlg2, invRegionAlg)

      def pre2 : RegionAlg[EvalInv, Eval with InvRegion[EvalInv]] = pre[EvalInv]

      def o = makeRegion(closeS(pre2))

    println("Is (0.5,0.5) inside it? " + o.eval(0.5, 0.5))

      // method 2: merge by library
      def evalInvAlg = combine[Eval, InvRegion[EvalInv], EvalInv](evalRegionAlg2, invRegionAlg)
      def o2 = makeRegion(closeS(evalInvAlg))
    println("Is (0.5,0.5) inside it? " + o.eval(0.5, 0.5))

    println()
  }

  // Testing 
  def main(args : Array[String]) {
    //    test1()
    //    test2()
    //    test3()
    //    test4()
    //    test5()
    test6()
  }

  def makeRegion[R](alg : RegionAlg[R, R]) = { import alg._; union(circle(1.0), circle(1.0)) }

  def test1() = {
    println(">>> Testing EvalRegionAlg")
    val o = makeRegion(evalRegionAlg)
    println("Is (1.0, 3.0) inside it? " + o.eval(1, 3))
    println()
  }

  trait RegionMerge[S, A, B] extends RegionAlg[S, A with B] {
    val lift : A => B => A with B
    val alg1 : RegionAlg[S, A]
    val alg2 : RegionAlg[S, B]

    def univ(x : Unit) : A with B = lift(alg1.univ({}))(alg2.univ({}))
    def circle(radius : Double) : A with B = lift(alg1.circle(radius))(alg2.circle(radius))
    def union(reg1 : S, reg2 : S) : A with B = lift(alg1.union(reg1, reg2))(alg2.union(reg1, reg2))
  }

  def regionMerge[S, A, B](mix : A => B => A with B, a1 : RegionAlg[S, A], a2 : RegionAlg[S, B]) : RegionAlg[S, A with B] =
    new RegionMerge[S, A, B] {
      val lift = mix
      val alg1 = a1
      val alg2 = a2
    }

  def mixEvalInv[In] : Eval => InvRegion[In] => Eval with InvRegion[In] = a => b => new Eval with InvRegion[In] {
    val fromUniv = b.fromUniv
    val fromCircle = b.fromCircle
    val fromUnion = b.fromUnion
    def eval = a.eval
  }

  def closeS[S <: A with B, A, B](alg : RegionAlg[S, A with B]) : RegionAlg[A with B, A with B] = alg.asInstanceOf[RegionAlg[A with B, A with B]]

  def test3() = {
    println(">>> Testing InvRegionAlg")
    object invAlg extends InvRegionAlg[InvRegion[Eval]]
    val o = { import invAlg._; univ() }
    println(o.fromUniv)
    println(o.fromUnion)
    println()
  }
}