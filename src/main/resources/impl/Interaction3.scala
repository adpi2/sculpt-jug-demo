import conway.{Cell, Dead, Interaction, Live}

object Interaction3 extends Interaction  {
  def interact(cell: Cell, neighbors: Seq[Cell]): Cell = {
    neighbors.count(_ == Live) match {
      case 2 | 3 => Live
      case _ => Dead
    }
  }
}
