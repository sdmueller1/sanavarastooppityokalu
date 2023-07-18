import SuomenKieli.*
import SanaOps.*
import scala.util.matching.Regex

@main def OpsTest():Unit =
  val test:Verbi = Verbi("nukkua")
  println(test.presentti)
  println(test.imperfekti)

