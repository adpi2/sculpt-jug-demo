import conway.{Dead, Interaction, Life, Live, Neighborhood, World}

class Life1[W](world: World[W], neighborhood: Neighborhood[W], interaction: Interaction) extends Life[W] {
  import world._
  import neighborhood._
  import interaction._

  override def next(w: W): W = {
    val positions = for { i <- 0 until w.width; j <- 0 until w.height } yield (i, j)
    positions.foldLeft(w){ case (agg, (i, j)) =>
      w.cell(i, j).interact(w.neighbors(i, j)) match {
        case Live => agg.populate(i, j)
        case Dead => agg
      }
    }
  }
}
