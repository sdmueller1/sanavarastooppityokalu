import scala.math.*
import scala.io.Source
import scala.util.matching.Regex
import scala.util.Random

//class dataWord(val text:String,var score:Double, var recency:Double):
//  def prob:Double = defnorm(score) * (1 - recencyBias) + recencyBias * recency
//  def reduceRecency():Unit = recency *= 0.95
//  override def toString:String = f"$text\n\tScore: $score%.3f\n\tRcncy: $recency%.3f\n\tPrbty: $prob%.3f"
//class WordPicker:
//  val r:Random = Random()
//  var words: Vector[dataWord] = Vector[dataWord]()
//  var j = 0
//  def getData: Unit =
//    val file = Source.fromFile(filepath)
//    val lines = file.getLines()
//    for line <- lines do
//      val parts = line.split("---")
//      words :+= dataWord(parts(0),parts(1).toDouble,parts(2).toDouble)
//  def chooseNext:String =
//    val probs = words.map(_.prob)
//    val probsum = probs.sum
//    val p = r.nextDouble()
//    var total = 0.0
//    for i <- probs.indices.filterNot(_==j) do
//      j = i
//      total += probs(i) / probsum
//      if p <= total then return words(i).text
//    "FAILED"
//  def prevCorrect(b:Boolean):Unit =
//    words.foreach(_.reduceRecency())
//    words(j).recency = 1
//    if b then
//      words(j).score += gainCoeff
//    else
//      words(j).score += lossCoeff
//  override def toString():String =
//    "Most probable words:\n" + "-"*20+"\n" +
//    words.sortBy(_.prob).takeRight(5).reverse.mkString("\n")

