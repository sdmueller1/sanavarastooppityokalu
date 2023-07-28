package SuomenKieli
import SanaOps.*
import SanaOps.KPTEngine.*
import scala.util.matching.Regex
def gradate(s:String) = s
class Sana(nominatiivi:String):
  val self:TypedWord = nominatiivi match {
    case wtCg(s) => typeCWord(nominatiivi,s)
    case wtBg(s,m,e) => typeBWord(nominatiivi)
    case wtAg(s,m,e) => typeAWord(nominatiivi)
    case _ => typeAWord(nominatiivi)
  }
  val weakStem = self.weakStem
  val strongStem = self.strongStem

  class typeAWord(nominative:String) extends TypedWord(nominative):
    var weakStem = gradate(nominative)
    var strongStem = nominative
    println("A + " + weakStem)

  class typeBWord(nominative: String) extends TypedWord(nominative):
    var weakStem = nominative
    var strongStem = gradate(nominative)
    println("B + " + strongStem)


  class typeCWord(nominative:String,ending:String) extends TypedWord(nominative):
    var weakStem = nominative
    var strongStem = nominative
    println("C + " + strongStem)

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