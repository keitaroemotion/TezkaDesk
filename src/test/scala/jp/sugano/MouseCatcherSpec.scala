package jp.sugano

import org.specs2.mutable.Specification
import java.awt.MouseInfo

class MouseCatcherSpec extends Specification
{
  "Mouse Analysis" should {
     "Display Mouse Location" in {
       val location = MouseInfo.getPointerInfo().getLocation()
       println(location)
       "" == ""
     }
  }
}
