package SuomenKieli
import SanaOps.*

import scala.util.matching.Regex
// Matches type A words and captures the relevant syllable for gradation
val typeAregex = s"(.*)([$vowels][$consonants]{1,2}[aäoöuyi])$$".r
// Matches type A words and captures the relevant syllable for gradation
val typeBregex = s"(.*)([$vowels][$consonants]{1,2}[$vowels])([$consonants])$$".r
// Type C words never undergo gradation, so they are separated
val typeCregex = s".*(nen|[uyoö]s)$$".r
val strongMap: Map[Regex, String] = Map(
  "[^skht]k[^skht]".r -> "kk",
  "[^skht]p[^skht]".r -> "pp",
  "[^skht]t[^skht]".r -> "tt",
  "[^skht]d[^skht]".r -> "t",
  "[uy]v[uy]".r -> "uku",
  "[^skht]v[^skht]".r -> "p",
  ".nn.".r -> "nt",
  ".ng.".r -> "nk",
  ".mm.".r -> "mp",
  ".ll.".r -> "lt",
  ".rr.".r -> "rt",
  "lje".r -> "lke",
  "rje".r -> "rke",
  "hke".r -> "hke"
)
def matchregexp(r:Regex,)
def gradate(s:String):String = s match {
  case typeCregex(c) => s
  case typeBregex(s,m,e) =>
    println(s+"|"+m+"|"+e)
    strongMap.foreach((a,b)=>println(a.toString() + " " + m.matches(a.toString())))
    s+strongMap.collectFirst((k,v) => if(k.matches(m))Some(v)else None).getOrElse(m)+e
  case typeAregex(s,m) =>
    println(s+"|"+m)
    s
    //s+weakMap.collectFirst((k,v)=>if(k.matches(m))v).getOrElse(m)
  case _ => s
}
class Sana(nominatiivi:String):
  val self:TypedWord = nominatiivi match {
    case typeCregex(s) => typeCWord(nominatiivi,s)
    case typeBregex(_) => typeBWord(nominatiivi)
    case typeAregex(_) => typeAWord(nominatiivi)
    case _ => typeAWord(nominatiivi)
  }
  val weakStem = self.weakStem
  val strongStem = self.strongStem

  class typeAWord(nominative:String) extends TypedWord(nominative):
    var weakStem = gradate(nominative)
    var strongStem = nominative


  class typeBWord(nominative: String) extends TypedWord(nominative):
    var weakStem = nominative
    var strongStem = gradate(nominative)


  class typeCWord(nominative:String,ending:String) extends TypedWord(nominative):
    var weakStem = nominative
    var strongStem = nominative

  trait TypedWord(nominative:String):
    var weakStem:String
    var strongStem:String
//    def genitive:String
//    def essive:String
//    def partitive:String
//    def translative:String
//    def inessive:String
//    def elative:String
//    def illative:String
//    def adessive:String
//    def ablative:String
//    def allative:String
//    def abessive:String
//    def comitive:String
//    def instructive:String
//    def pluralGenitive: String
//    def pluralEssive: String
//    def pluralPartitive: String
//    def pluralTranslative: String
//    def pluralInessive: String
//    def pluralElative: String
//    def pluralIllative: String
//    def pluralAdessive: String
//    def pluralAblative: String
//    def pluralAllative: String
//    def pluralAbessive: String
//    def pluralComitive: String
//    def pluralInstructive: String
class Adjektiivi(text:String) extends Sana(text):
  text

class Adverbi(text:String) extends Sana(text):
  text