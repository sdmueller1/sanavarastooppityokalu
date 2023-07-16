import SuomenKieli.KieliOps.*

import scala.io.StdIn.*
import scala.io.Source
import org.apache.commons.math3.distribution.*

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.*

import scala.util.Random
@main def run2() =
  val dict = FileIO.readFile("src/wordscores.csv")
  while
    val lw = dict.nextWord
    val word = lw.word
    val FtoEng = r.nextDouble() > 0.5
    dict.tick()
    if FtoEng then
      println(word.text)
    else
      println(word.definitions(r.nextInt(word.definitions.length)))
    readLine()
    print((if FtoEng then word.definitions.mkString(" | ") else word.text) + "\nCorrect? (y/n): ")
    val a = readLine().toLowerCase
    a match
      case "y" => lw.use(true)
      case "yy" =>
        lw.use(true)
        println("Kirjoita suomeks x2")
        readLine()
        readLine()
      case "n" =>2
        lw.use(false)
        println("Kirjoita sanan suomeks kahdesti:")
        readLine()
        readLine()
        println("Write the word in English twice:")
        readLine()
        readLine()
      case _ => lw.use()
    a match
      case "r" => println(dict.activeToString())
      case _ =>
    a != "q"
  do()
  println("Learned words:" + dict.learnedWords.length
  )
  FileIO.writeFile("src/wordscores.csv", dict)
@main def run3() =
  var dict = FileIO.readFile("src/wordscores.csv")
  FileIO.writeFile("src/test.csv", dict)
  dict = FileIO.readFile("src/test.csv")
  var actives = 0
  for j <- 0 to 30000 do
    val w = dict.nextWord
    w.use(r.nextDouble() < w.testProb)
    w.tap()
    dict.tick()
    actives += dict.activelength
  val learnedLength = dict.learnedWords.length
  val activelength = dict.activeWords.length
  val averageactive = actives / 20000.0
  val averagetaps = dict.learnedWords.map(_.taps).sum / learnedLength.toDouble
  println("no of learned words: " + learnedLength)
  println("no of active words:  " + activelength)
  println("average active:      " + averageactive)
  println("average attempts:    " + averagetaps)
  FileIO.writeFile("src/test.csv", dict)
//@main def run4() =
//  var dict = FileIO.readDirtyFile("src/dictionary.txt")
//  FileIO.writeFile("src/wordscores.csv",LivingDictionary.fromMap(dict))

