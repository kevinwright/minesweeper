package lsug.scaladojo.minesweeper

import MatrixOps.Matrix
import Cell._

object GridOps {
  type Grid = Matrix[Cell]
  type Cells = Seq[Cell]

  //convert a string delimited with newlines into a List[List[Char]]
  def gridFromString(input : String) : Grid = {
    val rowStrings = input.split('\n').toList
    rowStrings map { _.toList map {charToCell(_)} }
  }

  //convert a string delimited with newlines into a List[List[State]]
  def statesGridFromString(input : String) : Matrix[State] = {
    val rowStrings = input.split('\n').toList
    rowStrings map { _.toList map {charToState(_)} }
  }

  def apply(g:Grid) = new GridOps(g)
  
  implicit def grid2gridOps(g:Grid) = apply(g)
}



class GridOps(grid: Matrix[Cell]) extends MatrixOps[Cell](grid) {

  import GridOps._
  import Cell._

  //calls the func for every group of three consecutive cells in a row
  //using the results to build the replacement row.  Then does the same
  //for columns.
  //
  //empty "padding" cells are provided at the start and end of
  def propagate(f: (Cell,Cell,Cell) => Cell) = {
    // add a 0 on either end of the row
    def padded(cells : Cells) = Cell(false, 0) +: cells :+ Cell(false, 0)

    //takes a size 3 list and calls f using each element as an argument
    def invokeF(cells: Cells) : Cell = cells match {
      case Seq(l,c,r) => f(l,c,r)
      case _ => error("this should never happen!")
    }

    //process the rows then the cols
    map2d(padded(_).sliding(3).map(invokeF).toList)
  }

  def numVisible = count(_.visible)
  def numMined = count(_.mined)
  def numFlagged = count(_.flagged)
  def numTotal = grid.length * grid(0).length
  def numNotVisible = numTotal - numVisible
  def numMinesRemaining = numMined - numFlagged

  def disclose = mapCells {_.copy(visible = true)}
  def flagAllMines = mapCells {cell => if (cell.mined) cell.copy(flagged = true) else cell}


  def toggleFlag(col : Int, row : Int) : Grid = at(col, row)(_.toggleFlag)


  def cascade : Stream[Grid] = {
    def spreadEmptiness(l:Cell, c:Cell, r:Cell) =
      if(l.emptyTainted || r.emptyTainted) c.copy(visible = true, adjacentEmpty=true)
      else c

    //cleans the adjacentEmpty flag after each pass to stop the "taint"
    //from overtaking the grid and revealing everything!
    def removeTaint(c:Cell) : Cell = c.copy(adjacentEmpty=false)

    //recurse until no more to reveal
    def recurse(prev:Grid) : Stream[Grid] = {
      if(prev.numNotVisible == prev.numMined)
        Stream(prev.flagAllMines.disclose)
      else {
        val current = prev.propagate(spreadEmptiness).mapCells(removeTaint)
        if (current.numVisible > prev.numVisible) current #:: recurse(current)
        else Stream.Empty
      }
    }

    grid #:: recurse(grid)
  }

  def reveal(col : Int, row : Int) : Stream[Grid] = {
    //reveal selected
    val current = at(col, row)(_.click)
    if(current(row)(col).state == StateExploded) Stream(current, current.disclose)
    else current.cascade
  }

  def calcTotals =
    propagate{(l,c,r) => c.copy(count=l.count + c.count + r.count)}

  def states : Matrix[Cell.State] =  grid mapCells (_.state)
}
