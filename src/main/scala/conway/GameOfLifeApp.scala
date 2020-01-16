package conway

import sculpt.runtime.SculptRuntime
import sculpt.runtime.scalatest.ScalaTestSupport
import sculpt.scalatest.Spec
import sculpt.source.Resource

class GameOfLifeApp[W](game: GameOfLife[W]) {
  import game._

  def start(): Unit = {
    var world = game.create(10, 10)
      .populate(3, 3)
      .populate(2, 3)
      .populate(2, 2)
      .populate(4, 3)
      .populate(5, 3)
      .populate(5, 4)

    for (_ <- 0 until 1000) {
      print("\u001b[2J")
      show(world)
      Thread.sleep(500)
      world = world.next
    }
  }

  def show(world: W): Unit = {
    val lines = for (j <- 0 until world.height) yield {
      (0 until world.width).map(i => world.cell(i, j)).map {
        case Live => 'X'
        case Dead => ' '
      }.mkString("")
    }

    lines.foreach(println)
  }
}

object GameOfLifeApp extends SculptRuntime(Resource("/impl")) with ScalaTestSupport with App {
  run[GameOfLife[T forSome { type T }], Spec](GameOfLife.potential) {
    new GameOfLifeApp(_).start()
  }
}
