package lsug.scaladojo.minesweeper
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite, Spec}
import GridOps._
import MatrixOps._


@RunWith(classOf[JUnitRunner])
class GridSpec extends FunSuite with ShouldMatchers {
  test("convert from string") {
    GridOps(TestGrids.empty) should have (
      'numVisible (0),
      'numMined (0),
      'numFlagged (0),
      'numTotal (7*7),
      'numNotVisible (7*7),
      'numMinesRemaining (0)
    )
  }

  test("reveal empty grid") {
    val cascade =TestGrids.empty.reveal(0,0)

    GridOps(cascade.last) should have (
      'numVisible (7*7),
      'numMined (0),
      'numFlagged (0),
      'numTotal (7*7),
      'numNotVisible (0),
      'numMinesRemaining (0)
    )
  }

  test("reveal open grid") {
    val cascade =TestGrids.simpleopen.reveal(0,0)

    GridOps(cascade.last) should have (
      'numVisible (7*7),
      'numMined (3),
      'numFlagged (3),
      'numTotal (7*7),
      'numNotVisible (0),
      'numMinesRemaining (0)
    )

    cascade.last.states should equal (TestGrids.simpleopenRevealed)
  }

  test("reveal closed grid") {
    val cascade = TestGrids.simpleclosed.reveal(0,0)

    GridOps(cascade.last) should have (
      'numVisible (7*7 - 9),
      'numMined (8),
      'numFlagged (0),
      'numTotal (7*7),
      'numNotVisible (9),
      'numMinesRemaining (8)
    )
    
    cascade.last.states should equal (TestGrids.simpleclosedRevealed)
  }

  test("cascade empty grid") {
    val cascade = TestGrids.empty.reveal(0,0).toList

    cascade.length should equal (8)

    GridOps(cascade(0)) should have ('numVisible (1))
    GridOps(cascade(1)) should have ('numVisible (4))
    GridOps(cascade(2)) should have ('numVisible (9))
    GridOps(cascade(3)) should have ('numVisible (16))
    GridOps(cascade(4)) should have ('numVisible (25))
    GridOps(cascade(5)) should have ('numVisible (36))
    GridOps(cascade(6)) should have ('numVisible (49))
    //extra last step represents the final "disclose" op
    GridOps(cascade(7)) should have ('numVisible (49))

  }

  test("cascade open grid") {
    val cascade = TestGrids.simpleopen.reveal(0,0).toList

    cascade.map(_.states) should equal (TestGrids.simpleopenCascade)
//    cascade foreach {grid => println("---"); println(grid.matrixToString)}

//    cascade.length should equal (12)
  }

}