import conway.{Cell, Dead, World}

object World1 extends World[(Int, Int)] {
  def create(width: Int, height: Int): (Int, Int) = (width, height)
  def width(w: (Int, Int)): Int = w._1
  def height(w: (Int, Int)): Int = w._2
  def populate(w: (Int, Int), x: Int, y: Int): (Int, Int) = w
  def cell(w: (Int, Int), x: Int, y: Int): Cell = Dead
}
