import scala.io.StdIn.*
import scala.io.Source
import org.apache.commons.math3.distribution.*
@main def run2() =
  val dirty = FileIO.readDirtyFile("src/dictionary.txt")
  val dict = LivingDictionary.fromMap(dirty)
  FileIO.writeFile("src/balls.txt", dict)
  println("#:##;;;#0.0#;;;#0.0".matches("^.*#:#.*#;;;#[-+]?[0-9]*\\.?[0-9]+#;;;#[-+]?[0-9]*\\.?[0-9]+$"))
  val dict2 = FileIO.readFile("src/balls.txt")
  dict2.randomWord.use()
  println(dict2.words.map(_.relevance).sorted)
  while
    val w = dict2.nextWord
    println(w)
    readLine() != "qw"
  do()



  /*val x = WordPicker()
  x.getData
  println("PROCESS:\n" +
    "1. Each time you press Enter, a Finnish word will appear.\n" +
    "2. Type your English translation to the console\n" +
    "3. Determine whether your answer was correct and tell the system.\n" +
    "4. Repeat.")
  while readLine("Press Enter to generate a word. Q to quit.") != "exit" do
    println(x.chooseNext + "\n" + "-"*15)
    readLine()
    val answer = readLine("Correct answer? (Y/N): ")
    x.prevCorrect(answer.toUpperCase() == "Y")
    println(x)
  print(x.j)*/

