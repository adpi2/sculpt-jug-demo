import conway.{Cell, Dead, Live, World}

case class State(width: Int, length: Int, cells: Set[(Int, Int)])

object World2 extends World[State] {
  def create(width: Int, height: Int): State = State(width, height, Set())
  def width(w: State): Int = w.width
  def height(w: State): Int = w.length
  def populate(w: State, x: Int, y: Int): State = w.copy(cells = w.cells + ((x, y)))
  def cell(w: State, x: Int, y: Int): Cell = if (w.cells.contains((x, y))) Live else Dead
}