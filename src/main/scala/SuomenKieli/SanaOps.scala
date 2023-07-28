package SuomenKieli
import scala.util.matching.Regex
object SanaOps:
  val consonants = "bcdfghjklmnpqrstvwxz"
  val vowels = "aäoöuyie"
  object verbTypes:
    val vt1 = s".*([$vowels][aä])$$".r
    val vt2 = ".*(d[aä])$".r
    val vt3 = s".*([$consonants]{2}[$vowels])$$".r
    val vt4 = ".*([aäoöuy]t[aä])$".r
    val vt5 = ".*(it[aä])$".r
    val vt6 = ".*(et[aä])$".r
  object wordTypes:
    val wtA = ".*([aäoöuyi])$".r
    val wtB = ".*(e|in|[aä]s|t[oö]n|t[aä]r|is)$".r
    val wtC = ".*(nen|[uy]s|[oö]s)$".r
  object KPTEngine:

    val strongMap: Map[String, String] = Map(
      "k" -> "kk",
      "p" -> "pp",
      "t" -> "tt",
      "d" -> "t",
      "v" -> "p",
      "nn" -> "nt",
      "ng" -> "nk",
      "mm" -> "mp",
      "ll" -> "lt",
      "rr" -> "rt"
    )
    val weakMap: Map[String, String] = Map(
      "kk" -> "k",
      "pp" -> "p",
      "tt" -> "t",
      "nt" -> "nn",
      "nk" -> "ng",
      "mp" -> "mm",
      "lt" -> "ll",
      "rt" -> "rr",
      "t" -> "d",
      "p" -> "v",
      "k" -> ""
    )
    // Some base strings built initially for convenience
    val basekpt = s"(.*)([$vowels][$consonants]{1,2}"
    val fullkpt = basekpt + s"[$vowels])"
    // Used for capturing the various small patterns within the relevant syllable
    val (reg1,reg2,reg3,reg4) =
      ("([^skht])([kptdv])([^skht])".r, "(.)(..)(.)".r, "(.)([lrh])[jk][ei]".r, "([uy])(k)([uy])".r)
    // Groups the syllables in nonverbs
    val (wtAg,wtBg,wtCg) =
      ((basekpt + "[aäoöuyi])$").r, (fullkpt + s"([$consonants])$$").r, s".*(nen|[uyoö]s)$$".r)
    // Groups the syllables in verbs
    val (vt1g,vt3g,vt4_6g) =
      ((fullkpt + "([aä])").r, (fullkpt + s"([$consonants]{2}[aä])").r, (fullkpt + "(t[aä])").r)
    def strengthenCluster(cluster:String):String = cluster match {
      case reg1(s, m, e) =>
        s + strongMap.getOrElse(m, m) + e
      case reg2(s, m, e) =>
        s + strongMap.getOrElse(m, m) + e
      case reg3(s, m) =>
        s + m + "ke"
      case _ => cluster
    }
    def weakenCluster(cluster:String):String = cluster match {
      case reg4(s, m, e) =>
        s + "v" + e
      case reg1(s, m, e) =>
        s + weakMap.getOrElse(m, m) + e
      case reg2(s, m, e) =>
        s + weakMap.getOrElse(m, m) + e
      case reg3(s, m) =>
        s + m + "je"
      case _ => cluster
    }
    def gradateWord(nominative:String) = nominative match {
      case wtCg(s) => nominative
      case wtAg(s,m) => s + weakenCluster(m)
      case wtBg(s,m,e) => s + strengthenCluster(m) + e
      case _ => nominative
    }
    def gradateVerb(infinitive:String) = infinitive match {
      case vt1g(s,m,e) => s + weakenCluster(m) + e
      case vt3g(s,m,e) => s + strengthenCluster(m) + e
      case vt4_6g(s,m,e) => s + strengthenCluster(m) + e
      case _ => infinitive
    }
  def back(c:Char):Boolean = "aou".contains(c)
  def isFront(s:String):Boolean = s.forall(!back(_))
  val btf:Map[Char,Char] = Map('O'->'ö','A'->'ä','U'->'y')
  def assimilate(text:String):String =
    if (isFront(text)) text.foldLeft("")((a,b)=>a+btf.getOrElse(b,b))
    else text.toLowerCase()


