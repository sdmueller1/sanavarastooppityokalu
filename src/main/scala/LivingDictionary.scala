import LivingDictionary.fromFile

import scala.math.*
import scala.util.Random
import org.apache.commons.math3.special.Beta

val r = Random()
def norm(mean:Double,std:Double,x:Double):Double = pow(E,-0.5*pow((x-mean)/std,2))
def sscore(x:Double):Double = Beta.regularizedBeta(x,0.5,2.5)
def rscore(x:Double):Double = Beta.regularizedBeta(x,7.5,2)

class LivingDictionary(var words:Vector[LivingWord]):
  def length:Int = words.length
  def activelength:Int = math.max(1,activeWords.length)
  def activeWords = words.filter(_.recency > 0)
  def learnedWords = words.filter(_.score == 1)
  def verbs:LivingDictionary =
    LivingDictionary(words.filter(_.isVerb()))
  def tick():Unit =
    activeWords.foreach(_.age())
  private def randomWord(set:Vector[LivingWord]) =
    set(r.nextInt(set.length))
  def randomWord:LivingWord = randomWord(words)
  def reviewWord:LivingWord = randomWord(learnedWords)
  def nextWord:LivingWord =
    if r.nextDouble() > 0.5 / activelength then
      val probs = activeWords.map(_.relevance)
      val probsum = probs.sum
      val p = r.nextDouble()
      var total = 0.0
      for i <- probs.indices do
        total += probs(i) / probsum
        if total >= p then
          return activeWords(i)
    randomWord
  private def report(filterList:Vector[LivingWord]):String =
    "Score\tRcncy.\tProb.\tWord\n" +
    filterList.sortBy(-_.score).map(_.toReportString).mkString("\n")
  def activeToString() = report(activeWords)
  def learnedToString() = report(learnedWords)
  override def toString() = report(words)
object LivingDictionary:
  def fromFile:LivingDictionary =
    FileIO.readFile("src/dictionary.txt")
  def fromMap(m:Map[String,Vector[String]]):LivingDictionary =
    LivingDictionary(m.toVector.map(n => LivingWord(DictWord(n._1, n._2))))


class LivingWord(val word:DictWord, var score:Double, var recency:Double):
  def this(word:DictWord) = this(word,0.3,0)
  def isVerb():Boolean =
    word.definitions.foldLeft(false)(_ | _.contains("to "))
  def use():Unit =
    recency = 1.0
  def use(mark:Boolean):Unit =
    recency = 1.0
    if mark then
      score += 0.05
    else
      score -= 0.1
    if score > 1 then recency = 0
    score = math.min(score,1.0)
    score = math.max(score,0.0)
  def age():Unit =
    recency -= 0.025
    recency = math.max(recency,0.001)
  def relevance:Double =
    if recency > 0 then
      val x = sscore(score)
      val y = rscore(score)
      (x + 2 * y) * 0.5 * x
    else
      0
  def checkAnswer(s:String):Boolean =
    word.definitions.foldLeft(false)(_ | _ == s)
  def toReportString:String =
    f"$score%.3f\t$recency%.3f\t$relevance%.3f\t${word.text}"
  def toFileString:String =
    word.toFileString + "#;;;#" + score + "#;;;#" + recency
  override def toString:String =
    word.toString

object LivingWord:
  def fromFileString(s:String):LivingWord =
    val parts = s.split("#;;;#")
    LivingWord(DictWord.fromFileText(parts(0)),parts(1).toDouble,parts(2).toDouble)