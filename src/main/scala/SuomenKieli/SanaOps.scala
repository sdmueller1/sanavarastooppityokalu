package SuomenKieli
import scala.util.matching.Regex
object SanaOps:
  val VerbType1Pattern:Regex = ".*(aa|ea|eä|ia|iä|oa|ua|yä|ää|öä)$".r
  val VerbType2Pattern:Regex = ".*d(a|ä)$".r
  val VerbType3Pattern:Regex = ".*(ll|nn|rr|st)(a|ä)$".r
  val VerbType4Pattern:Regex = ".*(ata|ätä|ota|ötä|uta|ytä)$".r
  val VerbType5Pattern:Regex = ".*(ita|itä)$".r
  val VerbType6Pattern:Regex = ".*(eta|etä)$".r
  def backMouthPosition(text:String):Boolean = text.matches(".*(a|o|u).*")
  def assimilateMouthPosition(child:String,parent:String):String =
    if backMouthPosition(parent) then
      child.replaceAll("o","ö")
        .replaceAll("a","ä")
        .replaceAll("u","y")
    else 
      child.replaceAll("ö", "a")
        .replaceAll("ä", "o")
        .replaceAll("y", "u")  
  
  val consonants = "bcdfghjklmnpqrstvwxz"
  val vowels = "aäoöuyie"
  val kptlook = s"[$vowels]{1,2}[$consonants]+".r
  // Given a weak string, spits out the strong equivalent
  var stw = (s"^[$consonants]*[$vowels]{1,2}pm".r, ".*".r)
  val immutable:Regex = ".*(nen|[uyoö]s)$".r

  def vahva(heikko:String):String =

    heikko match
      case immutable(_) => heikko
      case stw._1(s)  => s
  // Given a a strong string, spits out the weak equivalent
  def heikko(vahva:String):String = ""
