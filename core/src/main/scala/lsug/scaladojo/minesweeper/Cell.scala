package lsug.scaladojo.minesweeper


object Cell {
  val Minechar = '*'
  val Emptychar = '.'

  implicit def intToCell(n : Int) : Cell = Cell(false, n)

  implicit def charToCell(char : Char) : Cell = {
    if (char == Minechar) Cell(true, 1)
    else if (char == Emptychar) Cell(false, 0)
    else error("unrecognised cell character: [" + char + "]")
  }

  sealed abstract class State(val name:String, val char:Char)
  case object StateFlagged extends State("flagged", '¶')
  case object StateHidden extends State("hidden", '☐')
  case object StateEmpty extends State("empty", '.')
  case object StateMined extends State("mined", '*')
  case object StateFlaggedGood extends State("flaggedgood", '✓')
  case object StateFlaggedBad extends State("flaggedbad", '✕')
  case object StateExploded extends State("exploded", '#')
  sealed abstract class CountState(name:String, char:Char) extends State(name, char)
  case object StateCount1 extends CountState("count1", '1')
  case object StateCount2 extends CountState("count2", '2')
  case object StateCount3 extends CountState("count3", '3')
  case object StateCount4 extends CountState("count4", '4')
  case object StateCount5 extends CountState("count5", '5')
  case object StateCount6 extends CountState("count6", '6')
  case object StateCount7 extends CountState("count7", '7')
  case object StateCount8 extends CountState("count8", '8')
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

  def toggleFlag = {
    if (visible) this
    else this.copy(flagged = !(this.flagged))
  }

  def click = {
    if (flagged) this
    else this.copy(clicked=true, visible = true)
  }

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
    else if (mined) StateMined
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

  override def toString = state.char.toString
}