package lsug.scaladojo.minesweeper

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite, Spec}

@RunWith(classOf[JUnitRunner])
class CellSpec extends FunSuite with ShouldMatchers {
  test("empty behaviour") {
    var cell = new Cell(mined = false, count = 0, visible = true)
    cell.empty should equal (true)

    cell = cell.copy(mined = true)
    cell.empty should equal (false)
  }
}

