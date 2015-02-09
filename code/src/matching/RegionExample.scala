package matching

object RegionExample {

  trait RegionAlg[In, Out] {
    def univ() : Out
    def circle(radius : Double) : Out
    def union(reg1 : In, reg2 : In) : Out
  }

  trait Eval { def eval : (Double, Double) => Boolean }

  // simple eval region
  trait EvalRegionAlg[In <: Eval] extends RegionAlg[In, Eval] {
    def univ() : Eval = new Eval { def eval = (_, _) => true }
    def circle(radius : Double) = new Eval { def eval = (x, y) => x * x + y * y <= radius * radius }
    def union(reg1 : In, reg2 : In) = new Eval { def eval = (x, y) => reg1.eval(x, y) || reg2.eval(x, y) }
  }
  object evalRegionAlg extends EvalRegionAlg[Eval]

  // eval region that supports pattern matching
  trait EvalRegionAlg2[In <: InvRegion[In] with Eval] extends PatternRegionAlg[In, Eval] {
    def univ() : Eval = new Eval { def eval = (_, _) => true }
    def circle(radius : Double) = new Eval { def eval = (x, y) => x * x + y * y <= radius * radius }
    def union(reg1 : In, reg2 : In) = new Eval {
      def eval = (x, y) => (reg1, reg2) match {
        case (PUniv(_ : Unit), _) => true
        case (_, PUniv(_ : Unit)) => true
        case _                    => reg1.eval(x, y) || reg2.eval(x, y)
      }
    }
  }
  def evalRegionAlg2[In <: InvRegion[In] with Eval] : PatternRegionAlg[In, Eval] = new EvalRegionAlg2[In] {}

  trait EvalInv extends Eval with InvRegion[EvalInv]

  trait InvRegion[R] {
    val fromUniv : Option[Unit]
    val fromCircle : Option[Double]
    val fromUnion : Option[(R, R)]
  }

  trait InvRegionAlg[In] extends RegionAlg[In, InvRegion[In]] {
    def univ() = new InvRegion[In] {
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
  def invRegionAlg2[In] : RegionAlg[In, InvRegion[In]] = new InvRegionAlg[In] {}

  trait PatternRegionAlg[In <: InvRegion[In], Out] extends RegionAlg[In, Out] {
    object PUniv { def unapply(e : In) : Option[Unit] = e.fromUniv }
    object PCircle { def unapply(e : In) : Option[Double] = e.fromCircle }
    object PUnion { def unapply(e : In) : Option[(In, In)] = e.fromUnion }
  }

  trait RegionAST[R] {
    trait RExp {
      def acceptI(v : IVisitor) : R
    }
    case class Univ() extends RExp {
      def acceptI(v : IVisitor) : R = v.univ()
    }
    case class Circle(radius : Double) extends RExp {
      def acceptI(v : IVisitor) : R = v.circle(radius)
    }
    case class Union(reg1 : RExp, reg2 : RExp) extends RExp {
      def acceptI(v : IVisitor) : R = v.union(reg1.acceptI(v), reg2.acceptI(v))
    }
    type IVisitor <: RegionAlg[R, R]
  }

  trait RegionASTSealed[R] extends RegionAST[R] {
    type IVisitor = RegionAlg[R, R]
  }

  trait ReifyRegionWrapper[R] {
    val regAST : RegionAST[R]
    import regAST._
    trait ReifyRegionAlg[In <: /*InvRegion[In] with */ RExp] extends RegionAlg[In, RExp] {
      def univ() : RExp = Univ()
      def circle(radius : Double) : RExp = Circle(radius)
      def union(reg1 : In, reg2 : In) : RExp = Union(reg1, reg2)
    }
    trait OptimizeRegionAlg[In <: InvRegion[In] with RExp] extends PatternRegionAlg[In, RExp] {
      def univ() : RExp = Univ()
      def circle(radius : Double) : RExp = Circle(radius)
      def union(reg1 : In, reg2 : In) : RExp = (reg1, reg2) match {
        case (PUniv(_), _) => Univ()
        case (_, PUniv(_)) => Univ()
        case _             => Union(reg1, reg2)
      }
    }
    trait OptimizeInv extends RExp with InvRegion[OptimizeInv]
    def optimizeRegionAlg2[In <: InvRegion[In] with RExp] = new OptimizeRegionAlg[In] {}
  }

  // Testing 
  def main(args : Array[String]) {
    test1()
    test2()
    test3()
    test4()
  }

  def makeRegion[R](alg : RegionAlg[R, R]) = { import alg._; union(circle(1.0), circle(1.0)) }

  def test1() = {
    println(">>> Testing EvalRegionAlg")
    val o = makeRegion(evalRegionAlg)
    println("Is (1.0, 3.0) inside it? " + o.eval(1, 3))
  }

  trait RegionMerge[S, A, B] extends RegionAlg[S, A with B] {
    val lift : A => B => A with B
    val alg1 : RegionAlg[S, A]
    val alg2 : RegionAlg[S, B]

    def univ() : A with B = lift(alg1.univ)(alg2.univ)
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

  def test2() = {
    println(">>> Testing EvalRegionAlg2")

      def pre[In <: InvRegion[In] with Eval] : RegionAlg[In, InvRegion[In] with Eval] =
        regionMerge[In, Eval, InvRegion[In]](mixEvalInv, evalRegionAlg2, invRegionAlg2)

      def pre2 : RegionAlg[EvalInv, Eval with InvRegion[EvalInv]] = pre[EvalInv]

      def o = makeRegion(closeS(pre2))

    println("Is (0.5,0.5) inside it? " + o.eval(0.5, 0.5))
  }

  def test4() {
    println(">>> Testing ReifyRegionAlg")
    object wrapper extends ReifyRegionWrapper[Eval] { val regAST = new RegionASTSealed[Eval] {} }
    import wrapper._
    import regAST.RExp

    object reifyRegionAlg extends ReifyRegionAlg[RExp]
    val o = { import reifyRegionAlg._; union(circle(1.0), circle(1.0)) }
    println(o.acceptI(evalRegionAlg).eval(0.5, 1.5))

    object optimizeRegionAlg extends OptimizeRegionAlg[OptimizeInv]
    val o2 = { import optimizeRegionAlg._; univ() }
    println(o2.acceptI(evalRegionAlg).eval(100, 100))

    /* //error code TODO
      def mixRExpInv[In] : RExp => InvRegion[In] => RExp with InvRegion[In] = a => b => new RExp with InvRegion[In] {
        val fromUniv = b.fromUniv
        val fromCircle = b.fromCircle
        val fromUnion = b.fromUnion
        def acceptI(v) = a.acceptI(v)
      }

      def pre[In <: InvRegion[In] with RExp] : RegionAlg[In, InvRegion[In] with RExp] =
        regionMerge[In, RExp, InvRegion[In]](mixRExpInv, optimizeRegionAlg2, invRegionAlg2)
        * 
        */

  }

  def test3() = {
    println(">>> Testing InvRegionAlg")
    object invAlg extends InvRegionAlg[InvRegion[Eval]]
    val o = { import invAlg._; univ() }
    println(o.fromUniv)
    println(o.fromUnion)
  }
}