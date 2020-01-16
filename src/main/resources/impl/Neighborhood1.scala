import conway.{Cell, Dead, Neighborhood}

class Neighborhood1[W] extends Neighborhood[W] {
  override def neighbors(w: W, x: Int, y: Int): Seq[Cell] = Seq(Dead, Dead, Dead)
}
