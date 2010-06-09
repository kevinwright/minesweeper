package lsug.scaladojo.minesweeper
package snippet

import scala.xml.NodeSeq

import net.liftweb.util.Helpers

import Helpers._
import GridOps._
import MatrixOps._
import java.util.Random
import Cell._
import net.liftweb.common.{Full, Box}
import net.liftweb.http._
import js.JE.JsRaw
import js.JsCmd

class MineGrid {

  def randomGrid(w:Int, h:Int) : Grid = {
    val rnd = new Random
    def rndBool : Boolean = rnd.nextInt(10) <= 0
    def mkRow = List.fill(w){
      val b = rndBool
      Cell(mined=b, count=(if(b) 1 else 0))
   }
    List.fill(h)(mkRow)
  }

  def newGrid : Grid = randomGrid(10,10).calcTotals


  object sessionGrid extends SessionVar[Grid](newGrid)
  def grid = sessionGrid.is
  def grid_=(g:Grid) = sessionGrid.set(g)

//  <a href={uri}>{text}</a>

  def lclickFunc(x:Int, y:Int) : JsCmd = {
    println("Received (%d,%d)".format(x,y));
    JsRaw("alert(’Button2 clicked’)")
}

  def gridAsHtml =
    <div class="minegrid">{
      grid.states.zipWithCoords.map { row =>
        <div class="row">{
          row map {
            case (x, y, state) =>
              val lclick = "lclick(%d,%d)".format(x,y)
              val rclick = "rclick(%d,%d)".format(x,y)

              val lclick2 = SHtml.ajaxCall(Str("Button-2"), ajaxFunc2 _)._2
              val rclick2 = SHtml.ajaxCall(Str("Button-2"), ajaxFunc2 _)._2

              val id="cell_%d_%d".format(x,y)
              val classes = "cell " + state.name
              <a id={id} class={classes} href="#" onclick={lclick} oncontextmenu={rclick}>&nbsp;</a>
          }
        }</div>
      }
    }</div>

  object actionParam extends RequestVar[Box[String]](S.param("action"))
  object xParam extends RequestVar[Box[Int]](S.param("x").map(_.toInt))
  object yParam extends RequestVar[Box[Int]](S.param("y").map(_.toInt))

  def show = {
    actionParam.get match {
      case Full("reset") => grid = newGrid

      case Full("click") =>
        for(x <- xParam; y <- yParam) {
//          println("revealing:" + x + "," + y)
          val cascade = grid.reveal(x,y).toList
//          println("here come the grids")
//          cascade foreach {grid => println("---"); println(grid.matrixToString)}
//          println("and returning...")
          grid = cascade.last
        }

      case Full("flag") =>
        for(x <- xParam; y <- yParam) {
          grid = grid.toggleFlag(x, y)
        }

      case _ =>
    }
    gridAsHtml
  }

  def stats =
    <ul>
      <li>total: {grid.numTotal} ({grid.numVisible} revealed, {grid.numNotVisible} hidden)</li>
      <li>mined: {grid.numMined} ({grid.numFlagged} flagged, {grid.numMinesRemaining} remaining)</li>
    </ul>

  def reset = <a href="?action=reset">reset</a>
}