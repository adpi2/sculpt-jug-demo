import conway.{Cell, Dead, Neighborhood, World}

class Neighborhood3[W](world: World[W]) extends Neighborhood[W] {
  import world._

  override def neighbors(w: W, x: Int, y: Int): Seq[Cell] = {
    for {
      i <- x - 1 to x + 1 if i >= 0 && i < w.width
      j <- y - 1 to y + 1 if j >= 0 && j < w.height
      if (i, j) != (x, y)
    } yield w.cell(i, j)
  }
}
