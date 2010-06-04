package lsug.scaladojo.minesweeper
package snippet

import scala.xml.NodeSeq

import net.liftweb.http.{RequestVar, SessionVar, StatefulSnippet, S}
import net.liftweb.common.Box
import net.liftweb.util.Helpers

import Helpers._
import GridOps._

class MineGrid {
  private[this] val inputA =
    """|..*..........
       |.............
       |..*...*.***..
       |..*...*...*..
       |..*...*...*..
       |..*...*...*..
       |..*...*...*..
       |..*.......*..
       |..*.......*..
       |..*.......*..
       |..*.*.**..*..
       |.............
       |.............""".stripMargin

  private[this] val inputB =
    """|......*......
       |.............
       |..*....*..*..
       |...*....*....
       |....*.....*..
       |.....**...*..
       |......*......
       |.....*.*.....
       |....*.....*..
       |...*....**...
       |..*.......*..
       |.............
       |.............""".stripMargin

  private[this] val input = inputB

  object sessionGrid extends SessionVar[Grid](gridFromString(input).calcTotals)
  def grid = sessionGrid.is
  def grid_=(g:Grid) = sessionGrid.set(g)

//  <a href={uri}>{text}</a>

  def cellToTd(cell : Cell, rowIdx : Int, colIdx : Int) = {
    val href = "?clickedX="+{colIdx}+"&clickedY="+{rowIdx}
    <td><a href={href}>{cell.toString}</a></td>
  }

  def rowToTr(row : Seq[Cell], rowIdx : Int) =
    <tr><td><b>{rowIdx}</b></td>{row.view.zipWithIndex map {case (cell, colIdx) => cellToTd(cell, rowIdx, colIdx)} }</tr>

  def gridAsTable =
    <table>
      <tr><td/>{grid(0).indices map {i => <td><b>{i}</b></td>} }</tr>
      {grid.view.zipWithIndex map {case (row,rowIdx) => rowToTr(row, rowIdx)} }
    </table>

  object clickedX extends RequestVar[Box[Int]](S.param("clickedX").map(_.toInt))
  object clickedY extends RequestVar[Box[Int]](S.param("clickedY").map(_.toInt))
  object resetParam extends RequestVar[Boolean](S.param("reset").map(_.toBoolean) getOrElse false)

  def show = {
    if(resetParam)  grid = gridFromString(input).calcTotals

//    val paramX = S.param("clickedX") map (_.toInt)
//    val paramY = S.param("clickedY") map (_.toInt)
    for(x <- clickedX; y <- clickedY) grid = grid.reveal(x,y)
    gridAsTable
  }

  def reset = <a href="?reset=true">reset</a>
}