package SuomenKieli
import SanaOps.*


private val t1group = s"(.*)([$consonants]{1,2})([$vowels]{2})$$".r
private val t3group = s"(.*[$vowels])([$consonants]{1,2})([$vowels]{1,2}[$consonants]{2}[$vowels])$$".r
private val specialt3 = s"(.*[$vowels])([$vowels][$consonants]{2}[$vowels])$$".r
private def wt1(infinitive:String):String =
  infinitive match
    case t1group(s, m, e) =>
      s + weakMap.getOrElse(m, m) + e
    case _ => infinitive
private def st3(infinitive:String):String =
  infinitive match
    case t3group(s, m, e) =>
      s + strongMap.getOrElse(m, m) + e
    case specialt3(s, e) =>
      s + "k" + e
    case _ => infinitive
private def st5(infinitive:String):String = ""
private def st6(infinitive:String):String = ""
class Verbi(val text:String):
  private trait TypedVerb(text:String):
    //
    protected val defaultEndings =
      Vector("n","mme","t","tte","","vAt")
    protected def zipConcat(a:Vector[String],b:Vector[String]) =
      a.zip(b).map((c,d)=>c+d)
    def firstInfinitive:String = text
    //val secondInfinitive:String
    //val thirdInfinitive:String
    //val fourthInfinitive:String
    //val fifthInfinitive:String

    //val strongStem:String
    //val weakStem:String
    //val activePresentParticiple:String
    //val activePastParticiple:String
    //val passivePresentParticiple:String
    //val passivePastParticiple:String
    //val agentParticiple:String
    //val potentialMood:Vector[String]
    //val presentPassive:Vector[String]
    //val negativePresentPassive:Vector[String]
    //val imperfectPassive:Vector[String]
    //val negativeImperfectPassive:Vector[String]
    //val perfectPassive:String



    def present:Vector[String]
    def imperfect:Vector[String]
    //val conditional:Vector[String]
    //val perfect:Vector[String]

    //val imperative:String

    protected def imperfectize(stem:String):String =
      val rem = "(a|ä|e)$".r
      stem match
        case rem(_) => stem.init + "i"
        case _ => stem + "i"
    protected def conditional(stem:String) =
      val rem = "e$".r
      stem match
        case rem(_) => stem.init + "isi"
        case _ => stem + "isi"
    protected def perfect(stem:String) =
      ""
    //val PresentStem:TenseStem
  // Type 1 verbs end with a vowel and then a/ä

  private class type1(text:String) extends TypedVerb(text):
    def stemize(s:String):String = s.dropRight(1)
    val strongStem = stemize(text)
    val weakStem = stemize(wt1(firstInfinitive))
    private val defaultStrengths = Vector(weakStem,weakStem,weakStem,weakStem,strongStem,strongStem)
    val present = defaultStrengths.zip(defaultEndings.updated(4,strongStem.last)).map((a,b)=>a+b)
    val imperfect = zipConcat(defaultStrengths.map(imperfectize),defaultEndings)
    //val PresentStem = new TenseStem(WeakStem,StrongStem,endings=endings)
  // Type 2 verbs end with a vowel and then a/ä
  //private class type2(text: String) extends TypedVerb(text)
  //  val stem = text.dropRight(2)
  //  val PresentStem = TenseStem(stem)
  // Type 3 verbs end in two consonants plus a vowel
  /*private class type3(text: String) extends TypedVerb:
    val PresentStem = text.dropRight(2) + "e"
  // Type 4 verbs end in a positioned vowel + t(a/ä)
  private class type4(text: String) extends TypedVerb:
    val PresentStem = text.dropRight(2) + text.last
  // Type 5 verbs end in i + t(a/ä)
  private class type5(text: String) extends TypedVerb:
    val PresentStem = text.dropRight(2) + "tse"
  // Type 6 verbs end in e + t(a/ä)
  private class type6(text: String) extends TypedVerb:
    val PresentStem = text.dropRight(2) + "ne"*/
  private def getVerbType(text:String):TypedVerb =
    text match
      //case VerbType2Pattern(_) => type2(text)
      //case VerbType3Pattern(_) => type3(text)
      //case VerbType4Pattern(_) => type4(text)
      //case VerbType5Pattern(_) => type5(text)
      case _ => type1(text)
  private val self:TypedVerb = getVerbType(text)
  val presentti:Vector[String]     = self.present
  val imperfekti:Vector[String]    = self.imperfect
  //val perfekti:Vector[String]      = self.perfect
  //val konditionaali:Vector[String] = self.conditional

private class TenseStem(
                         WeakStem:String,
                         StrongStem:String,
                         strengths:Vector[Boolean] = Vector(true,true,true,true,true,true),
                         endings:Vector[Option[String]] = Vector(None,None,None,None,None,None)
                       ):
  def this(s:String) = this(s,s)
  private val PersonStems:Vector[String] = strengths.map(if (_) StrongStem else WeakStem)
  private val DefaultEndings:Vector[String] = Vector("n","mme","t","tte","",assimilateMouthPosition("vat",WeakStem))
  private val PersonEndings:Vector[String] = DefaultEndings.zip(endings).map(n=>n._2.getOrElse(n._1))
  private val PersonConjugations:Vector[String] = PersonStems.zip(PersonEndings).map(n => n._1 + n._2)
  def FirstPersonSingular:String  = PersonConjugations(0)
  def FirstPersonPlural:String    = PersonConjugations(1)
  def SecondPersonSingular:String = PersonConjugations(2)
  def SecondPersonPlural:String   = PersonConjugations(3)
  def ThirdPersonSingular:String  = PersonConjugations(4)
  def ThirdPersonPlural:String    = PersonConjugations(5)


