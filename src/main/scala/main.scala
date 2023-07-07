import scala.io.StdIn.*
@main def run():Unit =
  println("Welcome to Finnish Vocabulary Practice!")
  gameRunner()

def saveData():Unit =
  print("HI!")
def gameRunner():Unit =
  gameRunner("RST")
def gameRunner(s:String):Unit =
  s.toUpperCase() match
    case "RST" =>
      println("H for help, Q to quit.")
      println("Please enter the number for the mode you would like: ")
    case "H" =>
      println(
        """Here are the mode numbers:
          |Word translations
          | 1. All words
          | 2. Verbs
          | 3. Nouns
          | 4. Adjectives
          | 5. Adverbs
          |Please enter the number for the mode you would like: """.stripMargin
      )
    case "Q" =>
      print("Would you like to save any mastery from this session? (y/n):")
      readLine().toUpperCase() match
        case "Y" =>
          saveData()
        case "N" =>
        case _ => gameRunner("Q")
      println("Kiitos!")
      System.exit(69)
    case "1" =>
      //translateGame(1)
    case _ =>
      println("Command not recognized. Please try again: ")
    gameRunner(readLine())

