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
    results = results.toggleFlag(2,0)
    results = results.toggleFlag(2,1)
    results = results.toggleFlag(2,2)
    println(results)
    println("revealed = " + results.numVisible)
    println("---")
    results = results.reveal(0,0).last
    println(results)
    println("revealed = " + results.numVisible)
    println("---")
    println(results.disclose)
  }

}