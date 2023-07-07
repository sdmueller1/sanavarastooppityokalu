class LivingDictionary(var words:Vector[LivingWord]):
  def length = words.length
  def randomWord:LivingWord =
    words(mathengine.randIndex(length))
  def nextWord:LivingWord =
    val probs = words.map(_.relevance)
    val probsum = probs.sum
    val p = mathengine.randDouble()
    var total = 0.0
    for i <- probs.indices do
      total += probs(i) / probsum
      if total >= p then
        return words(i)
    println("ERROR")
    randomWord
object LivingDictionary:
  def fromFile:LivingDictionary =
    FileIO.readFile("src/dictionary.txt")
  def fromMap(m:Map[String,Vector[String]]):LivingDictionary =
    LivingDictionary(m.toVector.map(n => LivingWord(Word(n._1, n._2))))
