package matching

object Graphics {
  trait Region {
    def eval(x : Double, y : Double) : Boolean = true
    def transform : Region
  }

  case class Univ() extends Region {
    //def eval(x : Double, y : Double) : Boolean = true
    def transform : Region = Univ()
  }

  case class Circle(radious : Double) extends Region {
    def eval = null
    def transform : Region = Circle(radious)
  }

  case class Union(r1 : Region, r2 : Region) extends Region {
    def eval = null
    def transform : Region =
      (r1, r2) match {
        case (Univ(), _) => Univ()
        case (_, Univ()) => Univ()
        case _           => Union(r1, r2)
      }
  }

}