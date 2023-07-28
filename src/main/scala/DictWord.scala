import java.io.FileNotFoundException
import scala.util.matching.Regex
class DictWord(val text:String, val definitions:Seq[String]):
  def toFileString:String = text + "#:#" + definitions.mkString("###")
  override def toString:String = text + " - " + definitions.mkString(" | ")

object DictWord:
  def fromFileText(s:String):DictWord =
    val parts = s.split("#:#")
    DictWord(parts(0),parts(1).split("###").toVector)
  def fromText(s:String):DictWord =
    val parts = s.split("\t")
    DictWord(parts.head,parts(1).split(",").map(_.trim).toVector)

