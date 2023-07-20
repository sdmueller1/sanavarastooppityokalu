import SuomenKieli.*
import SanaOps.*
import scala.util.matching.Regex

@main def OpsTest():Unit =
  val verbs = FileIO.readFile("src/scores.csv")
    .words
    .map(n => Verbi(n.word.text))
    .filter(_.exists)
  verbs.foreach(n => println(n.toString + "\n" + n.presentti.mkString(" | ")))

