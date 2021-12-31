package ziotest

import zio._
import zio.Console._
import zio.Random._

import java.lang.System
import java.io.IOException
import ziotest.BuildInfo

sealed abstract case class Name private (name: String)

object Name {
    def make(name: String): Option[Name] =
        if (!name.isEmpty) Some(new Name(name) {}) else None
}

sealed abstract case class Guess private (guess: Char)

object Guess {
    def make(str: String): Option[Guess] =
        Some(str.toList).collect {
            case c :: Nil if c.isLetter => new Guess(c.toLower) {}
        }
}

sealed abstract case class Word private (word: String) {
    def contains(char: Char) = word.contains(char)
    val length: Int          = word.length
    def toList: List[Char]   = word.toList
    def toSet: Set[Char]     = word.toSet
}

val words = List("hallo", "ekskerement", "politi", "frokost", "beluga")

object Word {
    def make(word: String): Option[Word] =
        if (!word.isEmpty && word.forall(_.isLetter)) Some(new Word(word.toLowerCase) {}) else None
}

sealed abstract case class State private (name: Name, guesses: Set[Guess], word: Word) {
  def failuresCount: Int            = (guesses.map(_.guess) -- word.toSet).size
  def playerLost: Boolean           = failuresCount > 5
  def playerWon: Boolean            = (word.toSet -- guesses.map(_.guess)).isEmpty
  def addGuess(guess: Guess): State = new State(name, guesses + guess, word) {}
}

object State {
  def initial(name: Name, word: Word): State = new State(name, Set.empty, word) {}
}

sealed trait GuessResult
object GuessResult {
  case object Won       extends GuessResult
  case object Lost      extends GuessResult
  case object Correct   extends GuessResult
  case object Incorrect extends GuessResult
  case object Unchanged extends GuessResult
}

def getUserInput(message: String): ZIO[Console, IOException, String] = {
    putStrLn(message).flatMap(_ => getStrLn)
}

lazy val getName: ZIO[Console, IOException, Name] =
    for {
        input <- getUserInput("What's your name?")
        name <- ZIO.fromOption(Name.make(input)) <> (putStrLn("Invalid input. Please try again...") *> getName)
    } yield name

lazy val chooseWord: URIO[Random, Word] =
    for {
        index <- nextIntBounded(words.length)
        word <- ZIO.fromOption(words.lift(index).flatMap(Word.make)).orDieWith(_ => new Error("Boom!"))
    } yield word

val hangmanStages = Map[Int, String](
    0 -> """
            # 
            # 
            # 
            # 
            # 
            # 
            # ------------
            #""".stripMargin('#'),
    1 -> """
            # +
            # |
            # |
            # |
            # |
            # |
            # +-----------
            #""".stripMargin('#'),
    2 -> """
            # +
            # |
            # |      0
            # |     /|\
            # |      |
            # |     / \
            # +-----------
            #""".stripMargin('#'),
    3 -> """
            # +-------
            # |       
            # |      0
            # |     /|\
            # |      |
            # |     / \
            # +-----------
            #""".stripMargin('#'),
    4 -> """
            # +--+----
            # | /     
            # |/     0
            # |     /|\
            # |      |
            # |     / \
            # +-----------
            #""".stripMargin('#'),
    5 -> """
            # +--+----
            # | /    |
            # |/     0
            # |     /|\
            # |      |
            # |     / \
            # +-----------
            #""".stripMargin('#'),
    6 -> """
            # +--+----
            # | /    |
            # |/     |
            # |      0
            # |     \|/
            # |      |
            # +-+   / \
            #""".stripMargin('#')
)

def renderState(state: State): URIO[Console, Unit] = {
    /*
      --------
      |      |
      |      0
      |     \|/
      |      |
      |     / \
      -
  
      f     n  c  t  o
      -  -  -  -  -  -  -
      Guesses: a, z, y, x
    */
    val hangman = ZIO(hangmanStages(state.failuresCount)).orDieWith(_ => new Error("Boom!"))
    val word =
      state.word.toList
        .map(c => if (state.guesses.map(_.guess).contains(c)) s" $c " else "   ")
        .mkString
  
    val line    = List.fill(state.word.length)(" - ").mkString
    val guesses = s" Guesses: ${state.guesses.map(_.guess).mkString(", ")}"
  
    hangman.flatMap { hangman =>
        putStrLn(
            s"""
            #$hangman
            #
            #$word
            #$line
            #
            #$guesses
            #
            #""".stripMargin('#')
        ).orDieWith(_ => new Error("Boom 2!"))
    }
}

lazy val getGuess: ZIO[Console, IOException, Guess] =
  for {
    input <- getUserInput("What's your next guess?")
    guess <- ZIO.fromOption(Guess.make(input)) <> (putStrLn("Invalid input. Please try again...") *> getGuess)
  } yield guess

def analyzeNewGuess(oldState: State, newState: State, guess: Guess): GuessResult =
    if (oldState.guesses.contains(guess)) GuessResult.Unchanged
    else if (newState.playerWon) GuessResult.Won
    else if (newState.playerLost) GuessResult.Lost
    else if (oldState.word.contains(guess.guess)) GuessResult.Correct
    else GuessResult.Incorrect

def gameLoop(oldState: State): ZIO[Console, IOException, Unit] =
    for {
        guess <- renderState(oldState) *> getGuess
        newState = oldState.addGuess(guess)
        guessResult = analyzeNewGuess(oldState, newState, guess)
        _ <- guessResult match {
            case GuessResult.Won =>
                putStrLn(s"Congratulations ${newState.name.name}!")
            case GuessResult.Lost =>
                putStrLn(s"Sorry ${newState.name.name}! You lost! Word was: ${newState.word.word}") *>
                    renderState(newState)
            case GuessResult.Correct =>
                putStrLn(s"Good guess, ${newState.name.name}!") *>
                    gameLoop(newState)
            case GuessResult.Incorrect =>
                putStrLn(s"Bad guess, ${newState.name.name}!") *>
                    gameLoop(newState)
            case GuessResult.Unchanged =>
                putStrLn(s"${newState.name.name}, you already tried that letter!") *>
                    gameLoop(newState)
        }
    } yield ()

object Hangman extends zio.App { 
    def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] =
        if (args.headOption == Some("--version")) {
            println(s"${BuildInfo.name} v${BuildInfo.version}")
            System.exit(0)
        }

        (for {
            name <- putStrLn("Welcome to ZIO Hangman!") *> getName
            word <- chooseWord
            _ <- gameLoop(State.initial(name, word))
        } yield ()).exitCode
}
