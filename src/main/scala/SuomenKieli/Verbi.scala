package SuomenKieli
import SanaOps.KPTEngine.gradateVerb
import SanaOps.verbTypes.*
//private val consonantClusterGroup = s"([$consonants]{1,2})"
//private val wordStartGroup = s"(.*[$vowels])"
//private val t1end = s"([$vowels]{2})$$"
//private val t3end = s"([$vowels]+[$consonants]{2}[$vowels])$$"
//private val t4_6end = s"([$vowels][$consonants][$vowels])$$"
//private val rjereg = s"(.*)([rl][jk]e)([$vowels]?[$consonants]{0,2}[$vowels])".r
//
//private val t1reg = (wordStartGroup + consonantClusterGroup + t1end).r
//val t3reg = (wordStartGroup + consonantClusterGroup + t3end).r
//private val t3reg_2 = (wordStartGroup + t3end).r
//private val t4_6reg = (wordStartGroup + consonantClusterGroup + t4_6end).r
//private val t4_6reg_2 = (wordStartGroup + t4_6end).r
////private val t1group = s"(.*)([$consonants]{1,2})([$vowels]{2})$$".r
////private val t3group = s"(.*[$vowels])([$consonants]{1,2})([$vowels]{1,2}[$consonants]{2}[$vowels])$$".r
////private val specialt3 = s"(.*[$vowels])([$vowels][$consonants]{2}[$vowels])$$".r
//
//private def wt1(infinitive:String):String =
//  infinitive match
//    case rjereg(start,mid,end) =>
//      start + weakMap.getOrElse(mid,mid) + end
//    case t1reg(start, mid, end) =>
//      start + weakMap.getOrElse(mid, mid) + end
//    case _ => infinitive
//private def st3(infinitive:String):String =
//  infinitive match
//    case rjereg(start, mid, end) =>
//      start + strongMap.getOrElse(mid, mid) + end
//    case t3reg(start, mid, end) =>
//      start + strongMap.getOrElse(mid, mid) + end
//    case t3reg_2(start, end) =>
//      start + "k" + end
//    case _ => infinitive
//private def st4_6(infinitive:String):String =
//  infinitive match
//    case rjereg(start, mid, end) =>
//      start + strongMap.getOrElse(mid, mid) + end
//    case t4_6reg(start,mid,end) =>
//      start + strongMap.getOrElse(mid, mid) + end
//    case t4_6reg_2(start, end) =>
//      start + "k" + end
//    case _ => infinitive

class Verbi(val EnsiInfinitiivi:String):
  private trait TypedVerb(firstInfinitive:String):
    def stemize(s:String):String
    val weakStem:String
    val strongStem:String
    def infinitiveStem:String = stemize(firstInfinitive)
    def imperativeStem:String = firstInfinitive.init
    var endings = Vector("n","mme","t","tte","","vAt")
    def strengths = Vector.fill(6)(strongStem)

    def present: Vector[String] = zipConcat(strengths,endings)
    def imperfect: Vector[String] = zipConcat(strengths.map(imperfectize),endings)
    def potentialMood:Vector[String] = zipConcat(strengths.map(conditionalize),endings)
    def imperative:Vector[String] = Vector(
      present.head.init,
      imperativeStem+"kAA",
      imperativeStem+"kOOn",
      imperativeStem+"kOOt"
    )


    //val secondInfinitive:String
    def thirdInfinitive:String = strongStem + "mA"
    def fourthInfinitive:String = strongStem + "minen"
    def fifthInfinitive:String = strongStem + "mAisillA"

    def activePresentParticiple:String = strongStem + "vA"
    def passivePresentParticiple:String = weakStem + "tAvA"
    def activePastParticiple:String = infinitiveStem + "nUt"

    def passivePastParticiple:String = weakStem + "tU"
    def agentParticiple:String = thirdInfinitive
    //val presentPassive:Vector[String]
    //val negativePresentPassive:Vector[String]
    //val imperfectPassive:Vector[String]
    //val negativeImperfectPassive:Vector[String]
    //val perfectPassive:String




    //val conditional:Vector[String]
    //val perfect:Vector[String]

    //val imperative:String
    protected def zipConcat(a:Vector[String],b:Vector[String]) = a.zip(b).map((c,d)=>c+d)
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
    val weakStem = stemize(gradateVerb(text))
    override def strengths = Vector(weakStem,weakStem,weakStem,weakStem,strongStem,strongStem)
    endings = endings.updated(4,strongStem.takeRight(1))
  // Type 2 verbs end with a vowel and then a/ä
  private class type2(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2)
    val weakStem = stemize(text)
    val strongStem = stemize(text)
  // Type 3 verbs end in two consonants plus a vowel
  private class type3(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "e"
    val strongStem: String = stemize(gradateVerb(text))
    val weakStem: String = stemize(text)
  // Type 4 verbs end in a positioned vowel + t(a/ä)
  private class type4(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + s.last
    val weakStem = stemize(text)
    val strongStem = stemize(gradateVerb(text))
  // Type 5 verbs end in i + t(a/ä)
  private class type5(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "tse"
    val weakStem = stemize(text)
    val strongStem = stemize(text)
  // Type 6 verbs end in e + t(a/ä)
  private class type6(text: String) extends TypedVerb(text):
    def stemize(s:String) = s.dropRight(2) + "ne"
    val weakStem = stemize(text)
    val strongStem = stemize(gradateVerb(text))
  private def getVerbType(text:String):TypedVerb =
    text match
      case vt1(_) => type1(text)
      case vt2(_) => type2(text)
      case vt3(_) => type3(text)
      case vt4(_) => type4(text)
      case vt5(_) => type5(text)
      case vt6(_) => type6(text)
      case _ => null//type1("invalidää")
  private val self:TypedVerb = getVerbType(EnsiInfinitiivi)
  def exists:Boolean = self != null
  def strongStem:String = self.strongStem
  def weakStem:String = self.weakStem
  def presentti:Vector[String]     = self.present
  def imperfekti:Vector[String]    = self.imperfect
  //val perfekti:Vector[String]      = self.perfect
  //val konditionaali:Vector[String] = self.conditional
  override def toString:String = EnsiInfinitiivi



