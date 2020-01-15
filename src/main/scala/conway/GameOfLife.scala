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

object GameOfLife extends ScalaTestSculpting {
  val interaction = any[Interaction].satisfying(new InteractionSpec(_))
}
