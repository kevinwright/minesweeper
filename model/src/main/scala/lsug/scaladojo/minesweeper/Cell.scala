package lsug.scaladojo.minesweeper


object Cell {
  val Minechar = '*'
  val Emptychar = '.'
  val Hiddenchar = '☐'
  val Flaggedchar = '✓'
  val BadFlagchar = '✕'
  val MissedMinechar = '*'

  implicit def intToCell(n : Int) : Cell = Cell(false, n)

  implicit def charToCell(char : Char) : Cell = {
    if (char == Minechar) Cell(true, 1)
    else if (char == Emptychar) Cell(false, 0)
    else error("unrecognised cell character: [" + char + "]")
  }
}

case class Cell(
  mined : Boolean = false,
  count : Int = 0,
  revealed : Boolean = false,
  flagged : Boolean = false)
{
  import Cell._

  def revealedEmpty = revealed  && !mined && count == 0

  override def toString =
    if (!revealed) {
      if (flagged) Flaggedchar.toString
      else Hiddenchar.toString
    }
    else if (flagged && mined) Flaggedchar.toString
    else if (flagged && !mined) BadFlagchar.toString
    else if (!flagged && mined) MissedMinechar.toString
    else if (count == 0) Emptychar.toString
    else count.toString
}