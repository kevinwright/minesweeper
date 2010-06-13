package lsug.scaladojo.minesweeper
package snippet

import net.liftweb.util.Helpers

import Helpers._
import GridOps._
import MatrixOps._
import java.util.Random
import Cell._
import net.liftweb.http._
import js.JE
import js.JsCmd
import js.JsCmds._
import net.liftweb.common.{Empty, Full, Box}
import scala.util.matching.Regex
import xml.{Text, NodeSeq}

class MineGrid {

  val jsCtx = JsContext(Empty, Empty)
  object CellId {
    val CellIdRegex = """cell_(\d+)_(\d+)""".r
    def apply(x: Int, y: Int): String = "cell_%d_%d".format(x,y)
    def unapply(id:String): Option[(Int,Int)] = id match {
      case CellIdRegex(xstr,ystr) => Some((xstr.toInt, ystr.toInt))
      case _ => None
    }
  }
  
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

  def updateStatsCmd : JsCmd = {
    SetHtml("stat_total", Text(grid.numTotal.toString)) &
    SetHtml("stat_visible", Text(grid.numVisible.toString)) &
    SetHtml("stat_notvisible", Text(grid.numNotVisible.toString)) &
    SetHtml("stat_mined", Text(grid.numMined.toString)) &
    SetHtml("stat_flagged", Text(grid.numFlagged.toString)) &
    SetHtml("stat_remaining", Text(grid.numMinesRemaining.toString))
  }

  def cellLeftClick(id : String) : JsCmd = {
    //println("lclick (%s)".format(id));
    id match {
      case CellId(x,y) =>
        //sequence of grids in the cascade (including the start pos)
        val gridseq = (grid #:: grid.reveal(x,y)).toList
        //we only want the cell states (not the underlying representation)
        val stateseq = gridseq.map(_.states)

        //generate a sequence of "deltas"
        //a delta is a Matrix[Option[X]], where each cell is Some(new value) or None if it hasn't changed
        val deltaseq = (stateseq.sliding(2) map {case Seq(a,b) => a delta b}).toList

        //for each "delta"...
        val jsonseq = deltaseq map { delta =>
          //zip with the co-ords, then collapse the rows into one Seq
          val flatWithCoords = delta.zipWithCoords.flatten
          //drop all the nones, and convert to a CellId->State.name pair
          val idStatePairs = flatWithCoords.collect{ case (x,y,Some(state)) => (CellId(x,y), state.name)}
          //unzip into two lists
          val (ids,states) = idStatePairs.unzip

          val jsonIds = "\"ids\" : " + ids.mkString("['", "','", "']")
          val jsonStates = "\"states\" : " + states.mkString("['", "','", "']")

          "    {" + jsonIds + ",\n    " + jsonStates + "}"
        }

        grid = gridseq.last

        val rawscript = jsonseq.mkString("var seq = [\n", ",\n", "];\n cascadeSeq(seq);")
        //println(rawscript)

        //JE.JsRaw("alert('left clicked x=%d, y=%d')".format(x, y))
        JE.JsRaw(rawscript) & updateStatsCmd

      
      case _ =>
        Noop
    }
  }

  def cellRightClick(id : String) : JsCmd = {
    //println("rclick (%s)".format(id));
    id match {
      case CellId(x,y) =>
        val newGrid = grid.toggleFlag(x,y)
        grid = newGrid

        val rawscript = "flip('%s','%s');".format(CellId(x,y), newGrid(y)(x).state.name)
        println(rawscript)

        //JE.JsRaw("alert('left clicked x=%d, y=%d')".format(x, y))
        //JE.JsRaw("flip('%s','%s')".format(id, "flaggedgood"))
        JE.JsRaw(rawscript) & updateStatsCmd
      
      case _ =>
        Noop
    }
  }

  def scripts =
    <script type="text/javascript">
      function cellLeftClick(id) {{
        {SHtml.ajaxCall(JE.JsRaw("id"), jsCtx, cellLeftClick _)._2};
      }}
      function cellRightClick(id) {{
        {SHtml.ajaxCall(JE.JsRaw("id"), jsCtx, cellRightClick _)._2};
      }}
    </script>

  def gridAsHtml =
    <div class="minegrid">{
      grid.states.zipWithCoords.map { row =>
        <div class="row">{
          row map {
            case (x, y, state) =>
              val classes = "cell " + state.name
              <a id={CellId(x,y)} class={classes} href="#">&nbsp;</a>
          }
        }</div>
      }
    }</div>

  val actionParam = S.param("action")
  val xParam = S.param("x") map (_.toInt)
  val yParam = S.param("y") map (_.toInt)

  def show = {
    actionParam match {
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


  def stats (xhtml : NodeSeq) =
    bind("stat", xhtml,
      "total"      -> <span id="stat_total">{grid.numTotal}</span>,
      "visible"    -> <span id="stat_visible">{grid.numVisible}</span>,
      "notvisible" -> <span id="stat_notvisible">{grid.numNotVisible}</span>,
      "mined"      -> <span id="stat_mined">{grid.numMined}</span>,
      "flagged"    -> <span id="stat_flagged">{grid.numFlagged}</span>,
      "remaining"  -> <span id="stat_remaining">{grid.numMinesRemaining}</span>
    )

  def resetFunc : JsCmd = {
    grid = newGrid
    val jsonseq = grid.states.zipWithCoords.reverse map { row =>
      val idStatePairs = row map {case (x,y,state) => (CellId(x,y), state.name)}
      //unzip into two lists
      val (ids,states) = idStatePairs.unzip

      val jsonIds = "\"ids\" : " + ids.mkString("['", "','", "']")
      val jsonStates = "\"states\" : " + states.mkString("['", "','", "']")

      "    {" + jsonIds + ",\n    " + jsonStates + "}"
    }

    val rawscript = jsonseq.mkString("var seq = [\n", ",\n", "];\n cascadeSeq(seq);")
    //println(rawscript)

    //JE.JsRaw("alert('left clicked x=%d, y=%d')".format(x, y))
    JE.JsRaw(rawscript) & updateStatsCmd
  }

  def reset = SHtml.a(resetFunc _, Text("reset"))
}