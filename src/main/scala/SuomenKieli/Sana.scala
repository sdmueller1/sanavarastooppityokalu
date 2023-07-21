package SuomenKieli
import SanaOps.*
// Matches type A words and captures the relevant syllable for gradation
val typeAregex = s".*([$vowels][$consonants]{1,2}[aäoöuyi])$$".r
// Matches type A words and captures the relevant syllable for gradation
val typeBregex = s".*([$consonants]{1,2}[$vowels])[$consonants]$$".r
// Type C words never undergo gradation, so they are separated
val typeCregex = s".*(nen|[uyoö]s)$$".r
class Sana(nominatiivi:String):
  val self:TypedWord = nominatiivi match {
    case typeCregex(s) => typeCWord(nominatiivi,s)
    case typeBregex(_) => typeBWord(nominatiivi)
    case typeAregex(_) => typeAWord(nominatiivi)
    case _ => typeAWord(nominatiivi)
  }

  class typeAWord(nominative:String) extends TypedWord(nominative):
    println("A")


  class typeBWord(nominative: String) extends TypedWord(nominative):
    println("B")


  class typeCWord(nominative:String,ending:String) extends TypedWord(nominative):
    println("C")

  trait TypedWord(nominative:String):
    val weakStem:String
    val strongStem:String
    def genitive:String
    def essive:String
    def partitive:String
    def translative:String
    def inessive:String
    def elative:String
    def illative:String
    def adessive:String
    def ablative:String
    def allative:String
    def abessive:String
    def comitive:String
    def instructive:String
    def pluralGenitive: String
    def pluralEssive: String
    def pluralPartitive: String
    def pluralTranslative: String
    def pluralInessive: String
    def pluralElative: String
    def pluralIllative: String
    def pluralAdessive: String
    def pluralAblative: String
    def pluralAllative: String
    def pluralAbessive: String
    def pluralComitive: String
    def pluralInstructive: String
class Adjektiivi(text:String) extends Sana(text):
  text

class Adverbi(text:String) extends Sana(text):
  text