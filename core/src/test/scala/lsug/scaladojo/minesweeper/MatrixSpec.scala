package lsug.scaladojo.minesweeper
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.{FunSuite, Spec}
import MatrixOps._


@RunWith(classOf[JUnitRunner])
class MatrixSpec extends FunSuite with ShouldMatchers {
  test("coords") {
    val matrix = List(
      List("a", "b", "c"),
      List("d", "e", "f"),
      List("g", "h", "i"))

    val expected = List(
      List((0,0), (1,0), (2,0)),
      List((0,1), (1,1), (2,1)),
      List((0,2), (1,2), (2,2)))

    val coords = matrix.coords

    coords should equal (expected)
  }

  test("zipWithCoords") {
    val matrix = List(
      List("a", "b", "c"),
      List("d", "e", "f"),
      List("g", "h", "i"))

    val expected = List(
      List((0,0, "a"), (1,0, "b"), (2,0, "c")),
      List((0,1, "d"), (1,1, "e"), (2,1, "f")),
      List((0,2, "g"), (1,2, "h"), (2,2, "i")))

    val result = matrix.zipWithCoords

    result should equal (expected)
  }

  test("deepZip") {
    val matrix1 = List(
      List("a", "b", "c"),
      List("d", "e", "f"),
      List("g", "h", "i"))

    val matrix2 = List(
      List("A", "B", "C"),
      List("D", "E", "F"),
      List("G", "H", "I"))

    val expected = List(
      List(("a", "A"), ("b", "B"), ("c", "C")),
      List(("d","D"), ("e","E"), ("f","F")),
      List(("g","G"), ("h","H"), ("i","I")))

    val result = matrix1 deepZip matrix2

    result should equal (expected)
  }

  test("delta") {
    val matrix1 = List(
      List("a", "b", "c"),
      List("d", "e", "f"),
      List("g", "h", "i"))

    val matrix2 = List(
      List("A", "b", "c"),
      List("d", "E", "f"),
      List("g", "h", "I"))

    val expected = List(
      List(Some("A"), None, None),
      List(None, Some("E"), None),
      List(None, None, Some("I")))

    val result = matrix1 delta matrix2

    result should equal (expected)
  }

}