import SuomenKieli.*
@main def OpsTest() =
  val testVerb:Verbi = Verbi("kysyä")
  println(testVerb.Present.FirstPersonSingularStem)
