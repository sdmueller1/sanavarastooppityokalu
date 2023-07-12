package SuomenKieli
import scala.util.matching.Regex
object KieliOps:
  val consonants = "bcdfghjklmnpqrstvwxz"
  val vowels = "aäoöuyie"
  val kptlook = s"[$vowels]{1,2}[$consonants]+".r
  // Given a weak string, spits out the strong equivalent
  var stw = (s"^[$consonants]*[$vowels]{1,2}pm".r, ".*".r)
  val immutable:Regex = ".*(nen|[uyoö]s)$".r
  def vahva(heikko:String):String =

    heikko match
      case immutable(_) => heikko
      case stw._1(s)
  // Given a a strong string, spits out the weak equivalent
  def heikko(vahva:String):String = ""
