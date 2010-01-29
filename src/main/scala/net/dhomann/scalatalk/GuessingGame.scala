package net.dhomann.scalatalk


import actors.Actor
import actors.Actor._
import scala.Range.Inclusive
import scala.collection.mutable.HashMap

/**
 * A simple game implementation with one coordinator actor and a number of player actors.
 * In each round, the coordinator makes up a number and asks all players to guess one.
 * Once all players have responded with a guess, the coordinator determines the player whose
 * guess is closest to his number, and announces the winner to all players.
 *
 * If more than one player had the best guess, there is no winner.
 */
object GuessingGame extends Application with Actor {
  // these are the messages exchanged between the actors
  // the compiler can make sure matches are exhaustive, because the case classes are sealed
  private sealed abstract case class Message()
  private case class TakeGuess(max: Long, r: Actor)
  private case class Guess(value: Long, p: Player)
  private case class AnnounceWinner(winner: Option[Player])

  // type alias for the map of collected guesses
  private type GuessMap = HashMap[Player, Long]

  private class Player(name: String) extends Actor {
    override def toString = name

    def act() {
      // game loop never returns
      loop {
        react {
          case TakeGuess(max, sender) =>
            sender ! Guess(random(Max), this)

          case AnnounceWinner(Some(winner)) =>
            if (winner == this)
              println(this + ": Cool, I won!")
            else
              println(this + ": Damn!")

          case AnnounceWinner(None) =>
            println(this + ": Hmm. Let's try again")
        }
      }
    }
  }

  private val players = List((1 to 3).map((i:Int) => new Player("Player " + i)): _*)
  private val Max: Long = 100
  private var number: Long = 0
  private val guesses = new GuessMap()

  private def random(max: Long):Long =
    Math.round(Math.floor((max + 1) * Math.random))

  /**
   * Reset coordinator state for a new game.
   */
  private def startGame {
    guesses.clear
    number = random(Max)
    println("New game, new luck! The magic number is " + number)
    players.foreach(_ ! TakeGuess(random(Max), this))
  }

  def act() {
    players.foreach(_.start)

    // let's play
    startGame

    // coordinator game loop never returns
    loop {
      react {
        case Guess(value, player) => {
          println("Received guess " + value + " from " + player)
          guesses += (player -> value)

          // we have all guesses, determine winners and restart
          if (guesses.size == players.length) {
            var winners = new GuessMap()
            var closest: Long = java.lang.Long.MAX_VALUE

            guesses.foreach(guess => {
              val distance = Math.abs(guess._2 - number)
              if (distance < closest) {
                winners.clear
                winners += guess
                closest = distance
              }
            })

            val winner: Option[Player] = if (winners.size == 1) Some(winners.keys.next) else None
            println("Coordinator: ... and the winner is " + winner.getOrElse(None))
            players.foreach(_ ! AnnounceWinner(winner))

            Thread.sleep(2000)
            
            startGame
          }
        }
      }
    }
  }

  // start the coordinator (will call #act), which in turn will start all the players
  start
}
