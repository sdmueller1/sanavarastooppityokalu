import SuomenKieli.*
import SanaOps.*
import scala.util.matching.Regex

val t3r = s"(.*[$vowels])([$consonants]{1,2})([$vowels]{1,2}[$consonants]{2}[$vowels])$$".r
val t3r2 = s"(.*[$vowels])([$vowels][$consonants]{2}[$vowels])$$".r
val strongMap:Map[String,String] = Map(
  "k" -> "kk",
  "p" -> "pp",
  "t" -> "tt",
  "nn" -> "nt",
  "ng" -> "nk",
  "mm" -> "mp",
  "ll" -> "lt",
  "rr" -> "rt",
  "d" -> "t",
  "v" -> "p",
  "lj" -> "lk",
  "rj" -> "rk"
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
  "k" -> "",
  "lk" -> "lj",
  "rk" -> "rj"
)

def strong(stem: String): String =
  stem match
    case t3r(s,m,e) =>
      s + strongMap.getOrElse(m,m) + e
    case t3r2(s,e) =>
      s + "k" + e
    case _ => stem
@main def OpsTest() =
  val testVerb:Verbi = Verbi("kysyä")
  println(strong("jaella"))
