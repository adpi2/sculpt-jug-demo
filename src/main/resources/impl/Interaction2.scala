import conway.{Cell, Dead, Interaction, Live}

object Interaction2 extends Interaction {
  def interact(cell: Cell, neighbors: Seq[Cell]): Cell = {
    if (neighbors.count(_ == Live) >= 2) Live else Dead
  }
}
