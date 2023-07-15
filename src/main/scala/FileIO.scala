import scala.io.Source
import java.io.*
import com.opencsv.*
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
    val reader = new CSVReader(new FileReader(path))
    var data = Vector.empty[LivingWord]
    var next = reader.readNext()
    while next != null do
      data :+= LivingWord.fromCSVArray(next)
      next = reader.readNext()
    reader.close()
    LivingDictionary(data)
  def writeFile(path:String, dict:LivingDictionary):Unit =
    val writer = new CSVWriter(new FileWriter(path))
    dict.words.foreach(n => writer.writeNext(n.toCSVWrite))
    writer.close()
