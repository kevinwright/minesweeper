package lsug.scaladojo.minesweeper

object App {

  import GridOps._

  def main(args : Array[String]) {

    val input = """|..*..........
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

    val mines = gridFromString(input)
    println("input:")
    println(mines.disclose)

    var results = mines.calcTotals
    println("output:")
    results = results.flag(2,0)
    results = results.flag(2,1)
    results = results.flag(2,2)
    println(results)
    println("revealed = " + results.numRevealed)
    println("---")
    results = results.reveal(0,0)
    println(results)
    println("revealed = " + results.numRevealed)
    println("---")
    println(results.disclose)
  }

}