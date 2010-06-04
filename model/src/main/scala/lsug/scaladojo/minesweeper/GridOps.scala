package lsug.scaladojo.minesweeper

import MatrixOps.Matrix

object GridOps {
  type Grid = Matrix[Cell]
  type Cells = Seq[Cell]

  //convert a string delimited with newlines into a List[List[Char]]
  def gridFromString(input : String) : Grid = {
    import Cell.charToCell
    val rowStrings = input.split('\n').toList
    rowStrings map { _.toList map {charToCell(_)} }
  }

  implicit def grid2gridOps(g:Grid) = new GridOps(g)
}



class GridOps(grid: Matrix[Cell]) extends MatrixOps[Cell](grid) {

  import GridOps._

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

  def numRevealed = count(_.revealed)


  def disclose = mapCells {_.copy(revealed = true)}


  def flag(col : Int, row : Int) : Grid = mapCell(col, row)(_.copy(flagged = true))


  def reveal(col : Int, row : Int) : Grid = {

    def propFunc(l:Cell, c:Cell, r:Cell) = {
      if(l.revealedEmpty || r.revealedEmpty) c.copy(revealed = true)
      else c
    }

    //recurse until no more to reveal
    def cascade(prev:Grid) : Grid = {
      val current = prev.propagate(propFunc)
      if (current.numRevealed > prev.numRevealed) cascade(current)
      else current
    }

    //reveal selected
    val current = mapCell(col, row)(_.copy(revealed = true))
    cascade(current)
  }

  def calcTotals =
    propagate{(l,c,r) => c.copy(count=l.count + c.count + r.count)}


}
