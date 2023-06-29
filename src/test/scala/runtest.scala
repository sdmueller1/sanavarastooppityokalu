import scala.io.StdIn.*
import scala.io.Source
@main def run2() =
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
  val file = Source.fromFile("C:\\Users\\Stefan Mueller\\Downloads\\vocabimporty.txt")
  val lines = file
    .getLines()
    .map(_.trim.split(" = "))
    .toVector
    .groupBy(x => x.head)
    .map(x => x._2.maxBy(y => y.reverse.head.length).mkString("\t"))
    .filter(n => !n.contains("Sivu") && !n.contains("KAPPALE") && !n.replaceAll("\\s","").forall(_.isDigit))
  lines.foreach(println)
