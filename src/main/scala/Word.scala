class Word(text:String, definitions:Vector[String]):
  def toFileString:String = text + "#:#" + definitions.mkString("###")
  override def toString:String = text + "\n" + definitions.mkString(", ")

object Word:
  def fromFileText(s:String):Word =
    val parts = s.split("#:#")
    Word(parts(0),parts(1).split("###").toVector)
  def fromText(s:String):Word =
    val parts = s.split("\t")
    Word(parts.head,parts(1).split(",").map(_.trim).toVector)