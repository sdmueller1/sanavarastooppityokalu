package SuomenKieli

import SuomenKieli.Verbi.{fpp, t1pattern}

import scala.util.matching.Regex

trait Sana:
  def vartalo:String
trait Verbi(val text:String) extends Sana:
  def back:Boolean = Verbi.back(text)
  def vartalo:String
  def ImperfektiVartalo:String

  // FPS -> First person singular ->  minä
  // FPP -> First person plural ->    me
  // SPS -> Second person singular -> sinä
  // SPP -> Second person plural ->   te
  // TPS -> Third person singular ->  hän
  // TPP -> Third person plural ->    he
  private def fps(stem: String): String = stem + "n"

  private def fpp(stem: String): String = stem + "mme"

  private def sps(stem: String): String = stem + "t"

  private def spp(stem: String): String = stem + "tte"

  private def tps(stem: String): String = stem + stem.takeRight(1)
  private def tpp(stem: String): String = stem + "v" + (if back then "a" else "ä") + "t"
  // Present tense
  def fps:String = fps(vartalo)
  def fpp:String = fpp(vartalo)
  def sps:String = sps(vartalo)
  def spp:String = spp(vartalo)
  def tps:String = tps(vartalo)
  def tpp:String = tpp(vartalo)


  // IMPERFECT TENSE CONJUGATIONS
  // First person singular -> minä
  def fpsi:String
  // First person plural -> me
  def fppi:String
  // Second person singular -> sinä
  def spsi:String
  // Second person plural -> te
  def sppi:String
  // Third person singular -> hän
  def tpsi:String
  // Third person plural -> he
  def tppi:String
object Verbi:
  def back(text:String) = text.matches(".*(a|o|u).*")


  val t1pattern:Regex = ".*(aa|ea|eä|ia|iä|oa|ua|yä|ää|öä)$".r
  val t2pattern:Regex = ".*d(a|ä)$".r
  val t3pattern:Regex = ".*(ll|nn|rr|st)(a|ä)$".r
  val t4pattern:Regex = ".*(ata|ätä|ota|ötä|uta|ytä)$".r
  val t5pattern:Regex = ".*(ita|itä)$".r
  val t6pattern:Regex = ".*(eta|etä)$".r
  val outo:Regex = "".r
  def getType(s:String):Verbi =
    s match
      case t1pattern(_) => VerbiT1(s)
      case t2pattern(_) => VerbiT2(s)
      case t3pattern(_) => VerbiT3(s)
      case t4pattern(_) => VerbiT4(s)
      case t5pattern(_) => VerbiT5(s)
      case t6pattern(_) => VerbiT6(s)
      case _ => OutoVerbi(s)

/* Exceptions TBA:
Selvitä t4
Hävitä t4
hävetä t4
kiivetä t4
ruveta t4
todeta t4
hapata t6
loitota t6
helpota t6
parata t6
*/
class VerbiT1(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(1)
class VerbiT2(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(2)
  override def hän:String = vartalo
class VerbiT3(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(2) + "e"
class VerbiT4(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(2) + (if back then "a" else "ä")
class VerbiT5(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(2) + "tse"
class VerbiT6(text:String) extends Verbi(text):
  def vartalo:String = text.dropRight(2) + "ne"
class OutoVerbi(text:String) extends Verbi(text):
  def vartalo:String =
    ""
