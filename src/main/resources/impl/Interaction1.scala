import conway.{Cell, Dead, Interaction}

object Interaction1 extends Interaction {
  def interact(cell: Cell, neighbors: Seq[Cell]): Cell = Dead
}
