package lsug.scaladojo.minesweeper.android.tests

import junit.framework.Assert._
import _root_.android.test.AndroidTestCase

class UnitTests extends AndroidTestCase {
  def testPackageIsCorrect {
    assertEquals("lsug.scaladojo.minesweeper.android", getContext.getPackageName)
  }
}