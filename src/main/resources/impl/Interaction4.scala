import conway.{Cell, Dead, Interaction, Live}

object Interaction4 extends Interaction {
  def interact(cell: Cell, neighbors: Seq[Cell]): Cell = {
    (cell, neighbors.count(_ == Live)) match {
      case (Live, 2) | (Live, 3) | (Dead, 3) => Live
      case _ => Dead
    }
  }
}
