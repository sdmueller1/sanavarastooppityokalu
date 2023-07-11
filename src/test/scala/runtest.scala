import SuomenKieli.Verbi

import scala.io.StdIn.*
import scala.io.Source
import org.apache.commons.math3.distribution.*

import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets
import java.io.*
@main def run2() =
  //val dirty = FileIO.readDirtyFile("src/dictionary.txt")
  //var dict = LivingDictionary.fromMap(dirty)
  //FileIO.writeFile("src/balls.txt", dict)
  val dict = FileIO.readFile("src/scores.txt")
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
      case "r" => println(dict.report)
      case _ =>
    a != "q"
  do()
  FileIO.writeFile("src/scores.txt", dict)
@main def run3() =
  println("test".dropRight(1))
  val v = Verbi.getType("haluta")
  println(v.back)
  println(v.minä)