package lsug.scaladojo.minesweeper.android

import _root_.android.app.Activity
import _root_.android.os.Bundle
import android.widget.{LinearLayout, TextView}

class MainActivity extends Activity {
  override def onCreate(savedInstanceState: Bundle) {
    super.onCreate(savedInstanceState)
    setContentView(R.layout.main)
//    root = (LinearLayout) findViewById(R.id.root);
    
//    setContentView(new TextView(this) {
//      setText("hello, world")
//    })
  }
}