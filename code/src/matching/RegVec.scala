package matching
/*
import RegionExample._

object RegVecExample {
  /*
  trait ExtRegion[Vin, Vout, Rin, Rout] extends RegionAlg[Rin, Rout] {
    def scale(reg : Rin, vec : Vin) : Rout
  }
  */

  trait ExtRegionAlg[In, Out] extends RegionAlg[In, Out] {
    type Vector
    def scale(reg : In, vec : Vector) : Out
  }

  trait ExtRegionAST[R] extends RegionAST[R] {
    type VectorRep
    type IVisitor <: ExtRegionIVisitor[R] { type VRep = VectorRep }
    case class Scale(region : RExp, vector : VectorRep) extends RExp {
      def acceptI(v : IVisitor) : R = v.scale(region.acceptI(v), v.interpretVector(vector))
    }

  }

  trait ExtRegionIVisitor[R] extends ExtRegionAlg[R, R] {
    type VRep
    def interpretVector(vec : VRep) : Vector
  }

  trait EvalExtRegion[In <: Eval] extends EvalRegionAlg[In] with ExtRegionAlg[In, Eval] {
    type Vector <: (Double, Double)
    def scale(r : In, v : Vector) : Eval = new Eval {
      def eval = (x, y) => r.eval(x / v._1, y / v._2)
    }
  }

}
* 
*/
