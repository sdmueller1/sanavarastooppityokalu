import scala.math.*
import scala.util.Random
class LivingWord(val word:Word, var score:Double, var recency:Double):
  def this(word:Word) = this(word,0.5,0)
  def markCorrect():Unit =
    score += 0.05
    score = math.min(score,1)
    this.use()
  def markWrong():Unit =
    score -= 0.1
    score = math.max(score,0)
    this.use()
  def use():Unit =
    recency = 1
  def age():Unit =
    recency -= 0.025
    recency = math.min(recency,0)
  def relevance:Double =
    mathengine.sscore(score) / mathengine.rscore(recency)
  def toFileString:String =
    word.toFileString + "#;;;#" + score + "#;;;#" + recency
  override def toString:String =
    word.toString

object LivingWord:
  def fromFileString(s:String):LivingWord =
    val parts = s.split("#;;;#")
    LivingWord(Word.fromFileText(parts(0)),parts(1).toDouble,parts(2).toDouble)
