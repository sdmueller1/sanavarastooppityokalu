import scala.io.Source
import java.io.*
object FileIO:
  // For purpose of initial imports and such
  def readDirtyFile(path:String):Map[String, Vector[String]] =
    val file = Source.fromFile(path)
    val lines = file.getLines().toVector.filter(_.trim.nonEmpty)
    val predict = lines.groupBy(_.split("\t").head).mapValues(_.map(_.split("\t").reverse.head))
    val dict = predict.mapValues(n => n.flatMap(_.split(",").map(_.trim)))
    dict.toMap
  // For general IO of the program
  def readFile(path:String):LivingDictionary =
    val file = Source.fromFile(path)
    var lines = file.getLines().toVector
    println(lines.length + " original")
    lines = lines.filter(_.matches("^.*#:#.*#;;;#[-+]?[0-9]*\\.?[0-9]+#;;;#[-+]?[0-9]*\\.?[0-9]+$"))
    println(lines.length + " filtered")
    LivingDictionary(lines.map(LivingWord.fromFileString))
  def writeFile(path:String, dict:LivingDictionary):Unit =
    val file = File(path)
    val writer = PrintWriter(file)
    writer.write(dict.words.map(_.toFileString).mkString("\n"))
    writer.close()

