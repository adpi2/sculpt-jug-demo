package conway

import org.scalatest.{FreeSpec, Matchers}
import sculpt.scalatest.ScalaTestSculpting

sealed trait Cell
case object Dead extends Cell
case object Live extends Cell

trait Interaction { self =>
  def interact(cell: Cell, neighbors: Seq[Cell]): Cell

  implicit class InteractionExt(cell: Cell) {
    def interact(neighbors: Seq[Cell]): Cell = self.interact(cell, neighbors)
  }
}

class InteractionSpec(interaction: Interaction) extends FreeSpec with Matchers {
  import interaction._

  "Any live cell with fewer than two live neighbours dies, as if by underpopulation." in {
    Live.interact(Seq(Dead, Dead, Dead)) shouldBe Dead
    Live.interact(Seq(Dead, Live, Dead, Dead, Dead)) shouldBe Dead
  }

  "Any live cell with two or three live neighbours lives on to the next generation." in {
    Live.interact(Seq(Live, Live, Dead)) shouldBe Live
    Live.interact(Seq(Dead, Live, Dead, Live, Live)) shouldBe Live
  }

  "Any live cell with more than three live neighbours dies, as if by overpopulation." in {
    Live.interact(Seq(Live, Live, Live, Live)) shouldBe Dead
    Live.interact(Seq(Live, Live, Dead, Live, Live, Live, Dead)) shouldBe Dead
  }

  "Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction." in {
    Dead.interact(Seq(Live, Dead, Live, Live)) shouldBe Live
    Dead.interact(Seq(Dead, Live, Dead, Live, Live)) shouldBe Live
  }

  "All other dead cells stay dead" in {
    Dead.interact(Seq(Dead, Dead, Live, Live)) shouldBe Dead
    Dead.interact(Seq(Dead, Live, Live, Live, Dead, Live)) shouldBe Dead
  }
}

trait World[W] { self =>
  def create(width: Int, height: Int): W

  def width(w: W): Int
  def height(w: W): Int
  def populate(w: W, x: Int, y: Int): W
  def cell(w: W, x: Int, y: Int): Cell

  implicit class WorldExt(w: W) {
    val width = self.width(w)
    val height = self.height(w)
    def populate(x: Int, y: Int): W = self.populate(w, x, y)
    def cell(x: Int, y: Int): Cell = self.cell(w, x, y)
  }
}

class WorldSpec[W](world: World[W]) extends FreeSpec with Matchers {
  import world._

  "A new world has size (width, height)" in {
    world.create(10, 20).width shouldBe 10
    world.create(10, 20).height shouldBe 20
  }

  "A new world contains only dead cells" in {
    world.create(10, 10).cell(0, 0) shouldBe Dead
    world.create(4, 12).cell(3, 5) shouldBe Dead
  }

  "A populated cell is alive" in {
    world.create(6, 8).populate(2, 7).cell(2, 7) shouldBe Live
    world.create(12, 15).populate(8, 8).cell(8, 8) shouldBe Live
  }
}

object GameOfLife extends ScalaTestSculpting {
  val interaction = any[Interaction].satisfying(new InteractionSpec(_))

  def world[W] = any[World, W].satisfying(new WorldSpec(_))
}
