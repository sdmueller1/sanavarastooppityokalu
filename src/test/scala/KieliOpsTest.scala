import SuomenKieli.*
import SanaOps.*
import SanaOps.KPTEngine.*
import SanaOps.wordTypes.*
import scala.util.matching.Regex
@main def wordstruct():Unit =
  val words = FileIO.readFile("src/wordscores.csv").words.map(_.word.text).filterNot(_.contains(" "))
  val consmap = words.map(_.map(c => if vowels.contains(c) then 'V' else 'C'))
  val freg = "(C{0,2}V+C{1,2}V?)".r
  consmap.map {
    case freg(s) => s
    case _ => "BBBB"
  }.toSet.foreach(println)
  //val finalregex = s"(.*?)([$consonants]{0,2}[$vowels]{0,2}[$consonants]?)$$".r
  //words.take(30).map(_ match
  //  case finalregex(s,e) => println(s + "|" + e)
  //  case _ => println())

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
  ".lje".r -> "lke",
  ".rje".r -> "rke",
  ".hje".r -> "hke"
)

@main def OpsTest():Unit =
  val s = Verbi("nukkua")
  val s2 = Verbi("kysytä")
  println(s.imperfekti.map(assimilate))
  println(s2.imperfekti.map(assimilate))


