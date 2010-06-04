package lsug.scaladojo.minesweeper
package snippet

import scala.xml.NodeSeq
import net.liftweb.util.Helpers
import Helpers._

class HelloWorld {
  def howdy(in: NodeSeq): NodeSeq =
    Helpers.bind("b", in, "time" -> (new java.util.Date).toString)
}
