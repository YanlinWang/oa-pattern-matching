package matching

object Tmp {

}
/*
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
      def univ(x : Unit) : RExp = Univ()
      def circle(radius : Double) : RExp = Circle(radius)
      def union(reg1 : In, reg2 : In) : RExp = Union(reg1, reg2)
    }
    trait OptimizeRegionAlg[In <: InvRegion[In] with RExp] extends PatternRegionAlg[In, RExp] {
      def univ(x : Unit) : RExp = Univ()
      def circle(radius : Double) : RExp = Circle(radius)
      def union(reg1 : In, reg2 : In) : RExp = (reg1, reg2) match {
        case (univ(_), _) => Univ()
        case (_, univ(_)) => Univ()
        case _            => Union(reg1, reg2)
      }
    }
    trait OptimizeInv extends RExp with InvRegion[OptimizeInv]
    def optimizeRegionAlg[In <: InvRegion[In] with RExp] = new OptimizeRegionAlg[In] {}
  }

  def test4() {
    println(">>> Testing ReifyRegionAlg")
    object wrapper extends ReifyRegionWrapper[Eval] { val regAST = new RegionASTSealed[Eval] {} }
    import wrapper._
    import regAST.RExp

    object reifyRegionAlg extends ReifyRegionAlg[RExp]
    val o = { import reifyRegionAlg._; union(circle(1.0), circle(1.0)) }
    println(o.acceptI(evalRegionAlg).eval(0.5, 1.5))

    // fake optimizeRegionAlg (without concrete invRegionAlg
    object optimizeRegionAlg extends OptimizeRegionAlg[OptimizeInv]
    val o2 = { import optimizeRegionAlg._; univ() }
    println(o2.acceptI(evalRegionAlg).eval(100, 100))

      /* // method 1: merge by hand
      def mixRExpInv[In] : RExp => InvRegion[In] => RExp with InvRegion[In] = a => b => new RExp with InvRegion[In] {
        val fromUniv = b.fromUniv
        val fromCircle = b.fromCircle
        val fromUnion = b.fromUnion
        def acceptI(v) = a.acceptI(v)
      }

      def pre[In <: InvRegion[In] with RExp] : RegionAlg[In, InvRegion[In] with RExp] =
        regionMerge[In, RExp, InvRegion[In]](mixRExpInv, optimizeRegionAlg2, invRegionAlg2)
        */

      // method 2: merge by library
      def optimizeInvAlg = combine[RExp, InvRegion[OptimizeInv], OptimizeInv](optimizeRegionAlg, invRegionAlg)
      def o3 = makeRegion(closeS(optimizeInvAlg))
    println(o3.acceptI(evalRegionAlg).eval(100, 100))

    println()
  }

*/ 