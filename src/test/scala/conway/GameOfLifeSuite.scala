package conway

import sculpt.source.Resource
import sculpt.test.SculptSuite
import sculpt.test.scalatest.ScalaTestSupport

class GameOfLifeSuite extends SculptSuite(Resource("/impl")) with ScalaTestSupport {
  test(GameOfLife.potential)
}
