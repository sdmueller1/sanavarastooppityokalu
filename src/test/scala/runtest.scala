import SuomenKieli.KieliOps.*

import scala.io.StdIn.*
import scala.io.Source
import org.apache.commons.math3.distribution.*

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.*

import scala.util.Random
@main def run2() =
  val dict = FileIO.readFile("src/scores.csv").verbs
  while
    val w = dict.nextWord
    println("-"*15 + "\n" + w.word.text + "\n" + "-"*15)
    dict.tick()
    readLine()
    print(w.word.definitions.mkString(" | ") + "\nCorrect? (y/n): ")
    val a = readLine().toLowerCase
    a match
      case "y" => w.use(true)
      case "yy" =>
        w.use(true)
        println("Kirjoita suomeks x2")
        readLine()
        readLine()
      case "n" =>2
        w.use(false)
        println("Write the word in English twice:")
        readLine()
        readLine()
        println("Kirjoita sanan suomeks kahdesti:")
        readLine()
        readLine()
      case _ => w.use()
    a match
      case "r" => println(dict.activeToString())
      case _ =>
    a != "q"
  do()
  FileIO.writeFile("src/scores.csv", dict)
@main def run3() =
  var dict = FileIO.readFile("src/scores.csv")
  println(dict.length)
  FileIO.writeFile("src/test.csv", dict)
  for i <- 0 to 20 do
    dict = FileIO.readFile("src/test.csv")
    for j <- 0 to 1000 do
      val w = dict.nextWord
      w.use(r.nextDouble() < w.testProb)
      dict.tick()
    println(f"${dict.activelength}\t${dict.learnedWords.length}")
    FileIO.writeFile("src/test.csv", dict)
//  var total = 0
//  for i <- 0 to 1000 do
//    val w = dict.nextWord
//    dict.tick()
//    w.use(Random.nextDouble() < 0.8)
//    total += dict.activeWords.length
//  println(total / 1000.0)
//  println(dict.activeToString())
//  println(dict.learnedToString())
//
//  println("Learned words: " + dict.learnedWords.length)
