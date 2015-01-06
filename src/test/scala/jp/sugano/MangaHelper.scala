package jp.sugano

import org.specs2.mutable.Specification
import java.io.FileOutputStream
import com.itextpdf.text._
import com.itextpdf.text.pdf._
import scala.collection.immutable.Map
import scala.collection.immutable.List
import testhelper._
import scala.util.control.Exception._

class MangaHelperSpec extends Specification
with TextOp
{
  val pagesize = PageSize.B4

  val mm = 2.834645669
  val font_directory = "/Library/Fonts"

  // font class
  val arial_font = new Font(BaseFont.createFont(s"$font_directory/Arial.ttf", BaseFont.WINANSI, BaseFont.EMBEDDED), 12)
  val hiram_font = new Font(BaseFont.createFont(s"$font_directory/ヒラギノ明朝 Pro W3.otf", BaseFont.IDENTITY_V, BaseFont.EMBEDDED), 12)

  def stamp_a_font(texts:List[String], font:Font, board:PdfContentByte, location:(Float, Float)) = {
    texts.foldLeft(0){ (offset,line) =>
      ColumnText.showTextAligned(board, Element.ALIGN_LEFT, new Phrase(line, font), location._1-offset, location._2, 0)
      offset+20
    }
  }

  def createPDF(filename:String) {
    var document = new Document(pagesize, 36f, 72f, 108f, 180f)
    val writer = PdfWriter.getInstance(document, new FileOutputStream(filename))
    document.open()
    document.add(new Paragraph("Hello"))
    var board = writer.getDirectContent()
    val content = Map[List[String],(Float,Float)](
      List("あなた","東京営業所") -> (110,170),
      List("ぐるなび") -> (20,70),
      List("株式会社") -> (130,70)
    )
    content.foreach{ x => stamp_a_font(x._1, hiram_font, board, x._2) }
    document.close()
  }

  class Stat(val acc:String, val container:List[String], val num:Int) {

  }

  def scrumbleTextInput(texts:Array[Char], boxHeight:Float, fontsize:Double):Stat = {
    def isLastElement(stat:Stat, texts:Array[Char]):Boolean = {
      stat.num == texts.size-1
    }
    def getCharsNumberPerLine(boxHeight:Float, fontsize:Double):Int = {
      (boxHeight / fontsize).toInt
    }
    texts.foldLeft(new Stat("",List[String](), 0)){ (stat, chara) =>
      val acc = stat.acc + chara
      stat.acc.length == getCharsNumberPerLine(boxHeight, fontsize) match {
        case true => new Stat("", stat.container :+ acc, stat.num+1)
        case _ if isLastElement(stat,texts) => new Stat(acc, stat.container :+ acc, stat.num+1)
        case _  => new Stat(acc, stat.container, stat.num+1)
      }
     }

   }

    //elem   ..location 200 200  ..box 200 300    ..c あいうえおかきくけこさしすせそぬるぽそしてそこからのほげほげ
 // }

  "Pictura Modification" should {

    "read dsl" in {
      // break down each argument of config lines, convert it to the BaloonStat Object
      def crumbleTextToBallonStatus(c:String, text:String, font:Font):BalloonStat = {
        text.split("-").foreach{ line =>

          val seq = line.split(" ")
          var location = (0.0, 0.0)
          var box = (0.0, 0.0)

          var c = ""

          def mkTouple(seq:Seq[String]):(Double,Double) = {
            (seq(1).toDouble, seq(2).toDouble)
          }

          seq(0).trim match {
            case "location" => location = mkTouple(seq)
            case "c" => c = seq(1)
            case "box" => box = mkTouple(seq)
          }
        }
        // these lines must be removed later
        val location = (0.0, 0.0)
        val box = (0.0, 0.0)
        new BalloonStat(font, location, box, c)
      }

      val dsl = System.getProperty("config.resource", "src/test/resources/serif.dsl")
      val config = parseConfigLines(scala.io.Source.fromFile(dsl).getLines.toList)

      def abort(msg:String) = {
        throw new Exception(msg)
      }

      //Font
      def setFont(config:Map[String, List[String]]):FontStat = {
        def toKeyValuePair(line:String, denom:Char):(String, String) = {
          val lsplit = line.split(denom)
          allCatch opt { lsplit(0) -> lsplit(1) } getOrElse {
            abort(s"$line cannot be devided by $denom")
          }
        }
        def canBeSplitBy(denom:String, line:String):Boolean = {
          line.contains(denom) && line.trim != ""
        }
        val font_config = allCatch opt { config("font")(0) } getOrElse {
          abort("your configuration file does not have Font Config")
        }
        val contentArchive = font_config.split("-").foldLeft(Map[String, String]()){ (map, line) =>
          canBeSplitBy(" ",line) match {
            case true => map + toKeyValuePair(line, ' ')
            case _ => map
          }
        }
        new FontStat(contentArchive("mark"), contentArchive("location"), contentArchive("size").toDouble)
      }

      // respective ballon contents
      config("elem").foldLeft(List[BalloonStat]()){ (list, l) =>
        List[BalloonStat]()
        // draw here!!
      }
      "" == ""
    }

    "allocate text according to BoxSize"  in {
      val boxsize = (300,200)
      val line1 = "坂の上の雲企画第一部門コンシュ"
      val line2 = "マーむうみん谷の愉快な仲間たち"
      val line3 = "林間学校あああああぬるぽ"
      val text = s"${line1}${line2}${line3}"

      scrumbleTextInput(text.toCharArray, boxsize._2, 5*mm).container mustEqual List(line1,line2,line3)
    }

    "#jpeg to pdf" in {
      createPDF("sample001.pdf")
      "" == ""
    }
  }
}
