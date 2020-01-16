package conway

import org.scalatest.{FreeSpec, Matchers}
import sculpt.Potential
import sculpt.scalatest.{ScalaTestSculpting, Spec}

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
    val width: Int = self.width(w)
    val height: Int = self.height(w)
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

trait Neighborhood[W] { self =>
  def neighbors(w: W, x: Int, y: Int): Seq[Cell]

  implicit class NeighborhoodExt(w: W) {
    def neighbors(x: Int, y: Int): Seq[Cell] = self.neighbors(w, x, y)
  }
}

class NeighborhoodSpec[W](world: World[W], neighborhood: Neighborhood[W]) extends FreeSpec with Matchers {
  import neighborhood._
  import world._

  "A corner cell has 3 neighbors" in {
    world.create(4, 4).neighbors(0, 0) should have size 3
    world.create(4, 4).neighbors(3, 0) should have size 3
  }

  "A side cell has 5 neighbors" in {
    world.create(4, 4).neighbors(0, 2) should have size 5
    world.create(4, 4).neighbors(1, 0) should have size 5
  }

  "The neighbors are the cells that are horizontally, vertically, or diagonally adjacent" in {
    world.create(4, 4).populate(0, 0).populate(0, 1).populate(1, 2)
        .neighbors(1, 1) should contain theSameElementsInOrderAs Seq(Live, Live, Dead, Dead, Live, Dead, Dead, Dead)
  }
}

trait Life[W] { self =>
  def next(w: W): W

  implicit class LifeExt(w: W) {
    def next: W = self.next(w)
  }
}

class LifeSpec[W](world: World[W], neighborhood: Neighborhood[W], interaction: Interaction, life: Life[W]) extends FreeSpec with Matchers {
  import world._
  import neighborhood._
  import interaction._
  import life._

  "A cell of the next generation is the result of the interaction with its neighbors" in {
    val init = world.create(5, 6).populate(2, 2).populate(3, 4).populate(2, 1).populate(1, 1)
    init.next.cell(1, 2) shouldBe init.cell(1, 2).interact(init.neighbors(1, 2))
    init.next.cell(2, 2) shouldBe init.cell(2, 2).interact(init.neighbors(2, 2))
  }
}

case class GameOfLife[W](world: World[W], life: Life[W]) {
  def create(width: Int, height: Int): W = world.create(width, height)

  implicit class GameOfLifeExt(w: W) {
    val width: Int = world.width(w)
    val height: Int = world.height(w)
    def cell(x: Int, y: Int): Cell = world.cell(w, x, y)
    def populate(x: Int, y: Int): W = world.populate(w, x, y)
    def next: W = w.next
  }
}

object GameOfLife extends ScalaTestSculpting {
  def potential[W]: Potential[GameOfLife[W], Spec] = for {
    interaction <- any[Interaction].satisfying(new InteractionSpec(_))
    world <- any[World, W].satisfying(new WorldSpec(_))
    neighborhood <- any[Neighborhood, W].satisfying(new NeighborhoodSpec(world, _))
    life <- any[Life, W].satisfying(new LifeSpec(world, neighborhood, interaction, _))
  } yield {
    GameOfLife(world, life)
  }
}
