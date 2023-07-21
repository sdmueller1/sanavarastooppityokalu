package SuomenKieli
import SanaOps.*


private val consonantClusterGroup = s"([$consonants]{1,2})"
private val wordStartGroup = s"(.*[$vowels])"
private val t1end = s"([$vowels]{2})$$"
private val t3end = s"([$vowels]+[$consonants]{2}[$vowels])$$"
private val t4_6end = s"([$vowels][$consonants][$vowels])$$"
private val rjereg = s"(.*)([rl][jk]e)([$vowels]?[$consonants]{0,2}[$vowels])".r

private val t1reg = (wordStartGroup + consonantClusterGroup + t1end).r
val t3reg = (wordStartGroup + consonantClusterGroup + t3end).r
private val t3reg_2 = (wordStartGroup + t3end).r
private val t4_6reg = (wordStartGroup + consonantClusterGroup + t4_6end).r
private val t4_6reg_2 = (wordStartGroup + t4_6end).r
//private val t1group = s"(.*)([$consonants]{1,2})([$vowels]{2})$$".r
//private val t3group = s"(.*[$vowels])([$consonants]{1,2})([$vowels]{1,2}[$consonants]{2}[$vowels])$$".r
//private val specialt3 = s"(.*[$vowels])([$vowels][$consonants]{2}[$vowels])$$".r

private def wt1(infinitive:String):String =
  infinitive match
    case rjereg(start,mid,end) =>
      start + weakMap.getOrElse(mid,mid) + end
    case t1reg(start, mid, end) =>
      start + weakMap.getOrElse(mid, mid) + end
    case _ => infinitive
private def st3(infinitive:String):String =
  infinitive match
    case rjereg(start, mid, end) =>
      start + strongMap.getOrElse(mid, mid) + end
    case t3reg(start, mid, end) =>
      start + strongMap.getOrElse(mid, mid) + end
    case t3reg_2(start, end) =>
      start + "k" + end
    case _ => infinitive
private def st4_6(infinitive:String):String =
  infinitive match
    case rjereg(start, mid, end) =>
      start + strongMap.getOrElse(mid, mid) + end
    case t4_6reg(start,mid,end) =>
      start + strongMap.getOrElse(mid, mid) + end
    case t4_6reg_2(start, end) =>
      start + "k" + end
    case _ => infinitive

class Verbi(val text:String):
  private trait TypedVerb(text:String):
    def stemize(s:String):String
    val weakStem: String
    val strongStem: String

    def firstInfinitive:String = text

    def present: Vector[String] = zipConcat(defaultStrengths, defaultEndings)

    def imperfect: Vector[String] = zipConcat(defaultStrengths.map(imperfectize), defaultEndings)

    protected val defaultEndings =
      Vector("n","mme","t","tte","","vAt")
    protected def defaultStrengths =
      Vector.fill(6)(strongStem)
    protected def zipConcat(a:Vector[String],b:Vector[String]) =
      a.zip(b).map((c,d)=>c+d)

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




    //val conditional:Vector[String]
    //val perfect:Vector[String]

    //val imperative:String
    protected def imperfectize(stem:String):String =
      val rem = "(a|ä|e)$".r
      stem match
        case rem(_) => stem.init + "i"
        case _ => stem + "i"
    protected def conditionalize(stem:String) =
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
    private val t1strengths = Vector(weakStem,weakStem,weakStem,weakStem,strongStem,strongStem)
    override def present = zipConcat(t1strengths,defaultEndings.updated(4,strongStem.takeRight(1)))
    override def imperfect = zipConcat(t1strengths.map(imperfectize),defaultEndings)
  // Type 2 verbs end with a vowel and then a/ä
  private class type2(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2)
    val weakStem = stemize(text)
    val strongStem = stemize(text)
  // Type 3 verbs end in two consonants plus a vowel
  private class type3(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "e"
    val strongStem = stemize(st3(text))
    val weakStem: String = stemize(text)
  // Type 4 verbs end in a positioned vowel + t(a/ä)
  private class type4(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + s.last
    val weakStem = stemize(text)
    val strongStem = stemize(st4_6(text))
  // Type 5 verbs end in i + t(a/ä)
  private class type5(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "tse"
    val weakStem = stemize(text)
    val strongStem = stemize(text)
  // Type 6 verbs end in e + t(a/ä)
  private class type6(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "ne"
    val weakStem = stemize(text)
    val strongStem = stemize(st4_6(text))
  private def getVerbType(text:String):TypedVerb =
    text match
      case VerbType1Pattern(_) => type1(text)
      case VerbType2Pattern(_) => type2(text)
      case VerbType3Pattern(_) => type3(text)
      case VerbType4Pattern(_) => type4(text)
      case VerbType5Pattern(_) => type5(text)
      case VerbType6Pattern(_) => type6(text)
      case _ => null//type1("invalidää")
  private val self:TypedVerb = getVerbType(text)
  def exists:Boolean = self != null
  def strongStem:String = self.strongStem
  def weakStem:String = self.weakStem
  def presentti:Vector[String]     = self.present
  def imperfekti:Vector[String]    = self.imperfect
  //val perfekti:Vector[String]      = self.perfect
  //val konditionaali:Vector[String] = self.conditional
  override def toString:String = text

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


