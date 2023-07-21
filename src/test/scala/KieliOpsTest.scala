import SuomenKieli.*
import SanaOps.*
import scala.util.matching.Regex
@main def wordstruct():Unit =
  val words = FileIO.readFile("src/wordscores.csv").words.map(_.word.text).filterNot(_.contains(" "))
  val consmap = words.map(_.map(c => if vowels.contains(c) then 'V' else 'C'))
  val freg = "(C{0,2}V+C{1,2}V?)".r
  consmap.map {n => n match
    case freg(s) => s
    case _ => "BBBB"
  }.toSet.foreach(println)
  //val finalregex = s"(.*?)([$consonants]{0,2}[$vowels]{0,2}[$consonants]?)$$".r
  //words.take(30).map(_ match
  //  case finalregex(s,e) => println(s + "|" + e)
  //  case _ => println())

val strongMap: Map[Regex, String] = Map(
  "[^s]?k".r -> "kk",
  "[^s]?p".r -> "pp",
  "[^s]?t".r -> "tt",
  "nn".r -> "nt",
  "ng".r -> "nk",
  "mm".r -> "mp",
  "ll".r -> "lt",
  "rr".r -> "rt",
  "d".r -> "t",
  "v".r -> "p",
  "lje".r -> "lke",
  "rje".r -> "rke"
)
def strengthen(str:String) = str match
  //start,middle,end
  case t3reg(s,m,e) =>
    s+strongMap.collectFirst((k,v)=>if(k.matches(m))v).getOrElse(m)+e

@main def OpsTest():Unit =
  val s = Sana("tyttanesdfsdn")



