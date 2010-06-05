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

  sealed trait State
  case object StateFlagged extends State
  case object StateHidden extends State
  case object StateEmpty extends State
  case object StateMined extends State
  case object StateFlaggedGood extends State
  case object StateFlaggedBad extends State
  case object StateExploded extends State
  sealed trait CountState extends State
  case object StateCount1 extends CountState
  case object StateCount2 extends CountState
  case object StateCount3 extends CountState
  case object StateCount4 extends CountState
  case object StateCount5 extends CountState
  case object StateCount6 extends CountState
  case object StateCount7 extends CountState
  case object StateCount8 extends CountState
}



case class Cell(
  mined : Boolean = false,
  count : Int = 0,
  clicked : Boolean = false,
  visible : Boolean = false,
  flagged : Boolean = false,
  adjacentEmpty : Boolean = false)
{
  import Cell._

  val empty = visible  && !mined && count == 0
  val emptyTainted = adjacentEmpty || empty

  def toggleFlagged = this.copy(flagged = !(this.flagged))

  lazy val state : State =
    if (!visible) {
      if (flagged) StateFlagged
      else StateHidden
    } //visible states:
    else if (flagged) {
      if (mined) StateFlaggedGood
      else StateFlaggedBad
    }
    else if (mined && clicked) StateExploded
    else if (!flagged && mined) StateMined
    else if (count == 0) StateEmpty
    else count match {
      case 1 => StateCount1
      case 2 => StateCount2
      case 3 => StateCount3
      case 4 => StateCount4
      case 5 => StateCount5
      case 6 => StateCount6
      case 7 => StateCount7
      case 8 => StateCount8
      case _ => error("this should never happen")
    }

  override def toString =
    if (!visible) {
      if (flagged) Flaggedchar.toString
      else Hiddenchar.toString
    }
    else if (flagged && mined) Flaggedchar.toString
    else if (flagged && !mined) BadFlagchar.toString
    else if (!flagged && mined) MissedMinechar.toString
    else if (count == 0) Emptychar.toString
    else count.toString
}