package bootstrap.liftweb

import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import Helpers._

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("lsug.scaladojo.minesweeper")

    // Build SiteMap
    val entries =
      Menu(Loc("Home", List("index"), "Home")) ::
      Menu(Loc("ajaxBtn", List("ajaxBtn"), "ajaxBtn")) ::
      Nil
    LiftRules.setSiteMap(SiteMap(entries:_*))
  }
}

