package jp.sugano

import org.specs2.mutable.Specification
import testhelper.TextOp

class TextOpSpec extends Specification
with TextOp
{
  "Config Line Manipulation" should {
    val sample_line =  "elem -location 200 200 -box 200 200 -c Juno API is always mine"
    val sample_line2 = "elem -location 200 200 -box 200 200 -c Juno API is always mine"
    val sample_line3 = "elem -location 200 200 -box 200 200 -c Juno API is always mine"
    val sample_line4 = "font -box 200 200"
    val sample_list = List(sample_line, sample_line2, sample_line3, sample_line4)

    "divide a line into key-value pairs" in {
      val result = convertLineToKeyValuePair(sample_line)
      result._1 mustEqual "elem"
      result._2 mustEqual "-location 200 200 -box 200 200 -c Juno API is always mine"
    }

    "dictionary handling" in {
       val map1 = Map[String, List[String]](
         "key1" -> List("v11","v12","v13"),
         "key2" -> List("v21","v22","v23"),
         "key3" -> List("v31","v32","v33")
       )
       val alpha = ("key2", "ext1")
       "" == ""
    }

    def trimEdge(text:String):String = {
      val chars = text.toCharArray
      var i = -1
      chars.foldLeft(""){ (acc, c) =>
        i += 1
        c match {
          case ' ' if i == 0 => acc
          case ' ' if i == chars.length-1 => acc
          case _ => acc + c
        }
      }
    }

    "trimStart and trimEnd" in {
      val template = "this is a test text."
      val templateWithSpace = s" $template "
      trimEdge(templateWithSpace) mustEqual template
    }

    "convert assmebled key_value_pairs into dictionary" in {
      parseConfigLines(sample_list).foreach { x =>
        println(x)
      }
      "" == ""
    }
  }
}
