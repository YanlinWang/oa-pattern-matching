package matching

object RegionExample {

  trait RegionAlg[In, Out] {
    def Univ() : Out
    def Circle(radius : Double) : Out
    def Union(reg1 : In, reg2 : In) : Out
  }

  trait InvRegion[R] {
    val fromUniv : Option[Unit]
    val fromCircle : Option[Double]
    val fromUnion : Option[(R, R)]
  }

  trait InvRegionAlg[In] extends RegionAlg[In, InvRegion[In]] {
    def Univ() = new InvRegion[In] {
      val fromUniv = Some()
      val fromCircle = None
      val fromUnion = None
    }
    def Circle(radius : Double) = new InvRegion[In] {
      val fromUniv = None
      val fromCircle = Some(radius)
      val fromUnion = None
    }
    def Union(reg1 : In, reg2 : In) = new InvRegion[In] {
      val fromUniv = None
      val fromCircle = None
      val fromUnion = Some(reg1, reg2)
    }
  }

  trait PatternRegionAlg[In <: InvRegion[In], Out] extends RegionAlg[In, Out] {
    object Unive { def unapply(e : In) : Option[Unit] = e.fromUniv }
    object Circle { def unapply(e : In) : Option[Double] = e.fromCircle }
    object Union { def unapply(e : In) : Option[(In, In)] = e.fromUnion }
  }

  trait OptimizeRegion[In <: InvRegion[In], Out] extends PatternRegionAlg[In, Out] {
    def Union(reg1 : In, reg2 : In) : Out = (reg1, reg2) match {
      case (Unive(_ : Unit), _) => Univ()
      case (_, Unive(_ : Unit)) => Univ()
      case _                    => Union(reg1, reg2)
    }
  }

  trait EvalRegionAlg[In <: Eval] extends RegionAlg[In, Eval] {
    def Univ() = (_, _) => true
    def Circle(radius : Double) = (x, y) => x * x + y * y <= radius * radius
    def Union(reg1 : In, reg2 : In) = (x, y) => reg1(x, y) || reg2(x, y)
  }

  trait EvalRegionAlg2[In <: InvRegion[In] with Eval] extends PatternRegionAlg[In, Eval] {
    def Univ = (x, y) => true
    def Circle(radius : Double) = (x, y) => x * x + y * y <= radius * radius
    def Union(reg1 : In, reg2 : In) = (reg1, reg2) match {
      case (Unive(_ : Unit), _) => Univ()
      case (_, Unive(_ : Unit)) => Univ()
      case _                    => (x, y) => reg1(x, y) || reg2(x, y)
    }
  }

  type Eval = (Double, Double) => Boolean
  trait InvEval extends Eval with InvRegion[InvEval]

  trait RegionAST[R] { //regAST =>
    trait RExp {
      def acceptI(v : IVisitor) : R
      //def acceptE(v : EVisitor) : R
    }
    case class Univ() extends RExp {
      def acceptI(v : IVisitor) : R = v.Univ()
      //def acceptE[R](v : EVisitor[R]) : R = v.univ
    }
    case class Circle(radius : Double) extends RExp {
      def acceptI(v : IVisitor) : R = v.Circle(radius)
      //def acceptE[R](v : EVisitor[R]) : R = v.circle(radius)
    }
    case class Union(reg1 : RExp, reg2 : RExp) extends RExp {
      def acceptI(v : IVisitor) : R =
        v.Union(reg1.acceptI(v), reg2.acceptI(v))
      //def acceptE[R](v : EVisitor[R]) : R = v.union(reg1, reg2)
    }
    type IVisitor <: RegionAlg[R, R]
    //type EVisitor[R] <: RegionEVisitor[RExp, R]
  }
  trait RegionEVisitor[RExp, Region] {
    def univ : Region
    def circle(radius : Double) : Region
    def union(reg1 : RExp, reg2 : RExp) : Region
  }
  trait RegionASTSealed[RExp, R] extends RegionAST[R] {
    type IVisitor = RegionAlg[RExp, R]
    type EVisitor = RegionEVisitor[RExp, R]
  }

  // Testing 

  def main(args : Array[String]) {
    test1() // EvalRegionAlg
    test2() // EvalRegionAlg2    
  }

  def makeRegion[R](alg : RegionAlg[R, R]) = { import alg._; Union(Circle(1.0), Circle(1.0)) }

  def test1() = {
    object evalAlg extends EvalRegionAlg[Eval]
    val o = makeRegion(evalAlg)
    println("Is (1.0, 3.0) inside it? " + o(1, 3))
  }
  def test2() = {
    object areaAlg2 extends EvalRegionAlg2[InvEval] // areaAlg2 : RegionAlg[,Area]
    val o = { import areaAlg2._; Univ }
    println("Is (0.5,0.5) inside it? " + o(0.5, 0.5))
  }
}