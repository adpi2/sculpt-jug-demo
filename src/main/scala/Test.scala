import sculpt.Potential
import sculpt.scalatest.ScalaTestSculpting
import sculpt.scalatest.Spec

class Test extends ScalaTestSculpting {
  val potential: Potential.Any[Int, Spec] = Potential.Any[Int, Spec]
}
