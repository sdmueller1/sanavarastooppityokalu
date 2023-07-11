import java.io.FileNotFoundException
import scala.util.matching.Regex
class DictWord(val text:String, val definitions:Vector[String]):
  def toFileString:String = text + "#:#" + definitions.mkString("###")
  override def toString:String = text + "\n" + definitions.mkString(", ")

object DictWord:
  def fromFileText(s:String):DictWord =
    val parts = s.split("#:#")
    DictWord(parts(0),parts(1).split("###").toVector)
  def fromText(s:String):DictWord =
    val parts = s.split("\t")
    DictWord(parts.head,parts(1).split(",").map(_.trim).toVector)


trait Sana(val text:String):
  def vartalo:String
trait Verbi(val text:String) extends Sana:
  def back:Boolean = text.matches(".*(aou).*")
  def vartalo:String
  def fps:String = vartalo + "n"
  def fpp:String = vartalo + "mme"
  def sps:String = vartalo + "t"
  def spp:String = vartalo + "tte"
  def tps:String = vartalo + text.takeRight(1)
  def tpp:String = vartalo + "v" + (if back then "a" else "ä") + "t"
object Verb:
  val t1pattern:Regex = ".*(aa|ea|eä|ia|iä|oa|ua|yä|ää|öä)$".r
  val t2pattern:Regex = ".*d(a|ä)$".r
  val t3pattern:Regex = ".*(ll|nn|rr|st)(a|ä)$".r
  val t4pattern:Regex = ".*(ata|ätä|ota|ötä|uta|ytä)$".r
  val t5pattern:Regex = ".*(ita|itä)$".r
  val t6pattern:Regex = ".*(eta|etä)$".r
  def getType(s:String):Any =

    s match
      case t1pattern(s) => t1Verbi(s)
      case t2pattern(s) => t2Verbi(s)
      case t3pattern(s) => t3Verbi(s)
      case t4pattern(s) => t4Verbi(s)
      case t5pattern(s) => t5Verbi(s)
      case t6pattern(s) => t6Verbi(s)
      case _ => new FileNotFoundException

class t1Verbi(text:String) extends Verb(text):
  def vartalo:String = text.dropRight(1)
class t2Verbi(text:String) extends Verb(text):
  def vartalo:String = text.dropRight(2)
  override def tps:String = vartalo
class t3Verbi(text:String) extends Verb(text):
  def vartalo:String = ""
class t4Verbi(text:String) extends Verb(text):
  def vartalo:String = ""
class t5Verbi(text:String) extends Verb(text):
  def vartalo:String = ""
class t6Verbi(text:String) extends Verb(text):
  def vartalo:String = ""


