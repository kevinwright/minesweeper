package lsug.scaladojo.minesweeper
package snippet

import scala.xml.NodeSeq

import net.liftweb.http.{RequestVar, SessionVar, StatefulSnippet, S}
import net.liftweb.util.Helpers

import Helpers._
import GridOps._
import java.util.Random
import Cell._
import net.liftweb.common.{Full, Box}

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


  def randomGrid(w:Int, h:Int) : Grid = {
    val rnd = new Random
    def rndBool : Boolean = rnd.nextInt(10) <= 0
    def mkRow = List.fill(w){
      val b = rndBool
      Cell(mined=b, count=(if(b) 1 else 0))
   }
    List.fill(h)(mkRow)
  }

  def newGrid : Grid =
    //gridFromString(inputA).calcTotals
    randomGrid(10,10).calcTotals


  object sessionGrid extends SessionVar[Grid](newGrid)
  def grid = sessionGrid.is
  def grid_=(g:Grid) = sessionGrid.set(g)

//  <a href={uri}>{text}</a>

  def cellClass(cell : Cell) : String = cell.state match {
    case StateFlagged => "flagged"
    case StateHidden => "hidden"
    case StateEmpty => "empty"
    case StateMined => "mined"
    case StateFlaggedGood => "flaggedgood"
    case StateFlaggedBad => "flaggedbad"
    case StateExploded => "exploded"
    case StateCount1 => "count1"
    case StateCount2 => "count2"
    case StateCount3 => "count3"
    case StateCount4 => "count4"
    case StateCount5 => "count5"
    case StateCount6 => "count6"
    case StateCount7 => "count7"
    case StateCount8 => "count8"

  }
  
  def cellToHtml(cell : Cell, rowIdx : Int, colIdx : Int) = {
    val coords = "&x=" + colIdx + "&y=" + rowIdx
    val href = "?action=click"+coords
    val rclick = "location.href='?action=flag"+coords+"'; return false;"
    <a class={cellClass(cell)} href={href} oncontextmenu={rclick}>&nbsp;</a>
  }

  def rowToHtml(row : Seq[Cell], rowIdx : Int) =
    <div class="row">
      {row.view.zipWithIndex map {case (cell, colIdx) => cellToHtml(cell, rowIdx, colIdx)} }
    </div>

  def gridAsHtml =
    <div class="minegrid">
      {grid.view.zipWithIndex map {case (row,rowIdx) => rowToHtml(row, rowIdx)} }
    </div>

  object actionParam extends RequestVar[Box[String]](S.param("action"))
  object xParam extends RequestVar[Box[Int]](S.param("x").map(_.toInt))
  object yParam extends RequestVar[Box[Int]](S.param("y").map(_.toInt))

  def show = {
    actionParam.get match {
      case Full("reset") => grid = newGrid

      case Full("click") =>
        for(x <- xParam; y <- yParam) {
          val revealStream = grid.iterativeReveal(x,y)
          grid = revealStream.last
        }

      case Full("flag") =>
        for(x <- xParam; y <- yParam) {
          grid = grid.flag(x, y)
        }

      case _ =>
    }
    gridAsHtml
  }

  def reset = <a href="?action=reset">reset</a>
}