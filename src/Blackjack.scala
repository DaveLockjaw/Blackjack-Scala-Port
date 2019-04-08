import scala.util.Random
import scala.io.StdIn.readLine
import util.control.Breaks._
import scala.collection.mutable.ListBuffer

object Blackjack {

  def calculateScore(cards: List[Any]): Int = {
    var score: Int = 0
    var scoreInc: Int = 0
    var hasAce: Boolean = false

    for (i <- cards) {
      i match {
        case "2" => scoreInc = 2
        case "3" => scoreInc = 3
        case "4" => scoreInc = 4
        case "5" => scoreInc = 5
        case "6" => scoreInc = 6
        case "7" => scoreInc = 7
        case "8" => scoreInc = 8
        case "9" => scoreInc = 9
        case "10"  | "J" | "Q" | "K" => scoreInc = 10
        case "A" => hasAce = true
        case _ => println("card is nil")
      }

      if (hasAce) {
        if (score > 11)
          scoreInc = 1
        else
          scoreInc = 11
      }

      score += scoreInc
    }

    return score
  }

  def printStatus(playerCards: List[Any], dealerCards: List[Any]): Unit = {
    println("")
    println("Player's total is " + playerCards)

    playerCards.foreach {
      println
    }
    println("")

    println("Dealer's total is " + dealerCards)
    dealerCards.foreach {
      println
    }
    println("")
  }

  def populateDeck(): ListBuffer[String] = {
    var tmp = new ListBuffer[String]()

    tmp += "2"
    tmp += "3"
    tmp += "4"
    tmp += "5"
    tmp += "6"
    tmp += "7"
    tmp += "8"
    tmp += "9"
    tmp += "10"
    tmp += "J"
    tmp += "Q"
    tmp += "K"
    tmp += "A"

    return tmp
  }

  def main(args: Array[String]) = {
    var deckBuffer = populateDeck()
    var playerDeckBuffer = new ListBuffer[String]()
    var dealerDeckBuffer = new ListBuffer[String]()

    //Shuffle the deck randomly
    var shuffledDeck = Random.shuffle(deckBuffer.toList)
    println("Deck: " + shuffledDeck.mkString(", "))

    // dealer draws first card
    println("Dealer draws first card. \n")
    dealerDeckBuffer += shuffledDeck.last
    shuffledDeck = shuffledDeck.dropRight(1)
    println("Dealer's hand: " + dealerDeckBuffer.toList.mkString(", ") + "\n")

    println("Current deck: " + shuffledDeck.mkString(", ") + "\n")
    // player draws 2 cards
    println("Player receives two cards")
    playerDeckBuffer += shuffledDeck.last
    shuffledDeck = shuffledDeck.dropRight(1)
    playerDeckBuffer += shuffledDeck.last
    shuffledDeck = shuffledDeck.dropRight(1)
    println("Your hand: " + playerDeckBuffer.toList.mkString(", ") + "\n")

    var playerDraw: Boolean = true
    var quit: Boolean = false
    var playerBUSS = false
    var dealerBUSS = false

    while (playerDraw) { // player decision loop
      println("Do you want to (H)it", "(S)tay, or (Q)uit")
      val selection = readLine()
      val upperCaseSelection = selection.toUpperCase()

      if (upperCaseSelection == "H") { //hit
        playerDeckBuffer += shuffledDeck.last
        shuffledDeck = shuffledDeck.dropRight(1)

        println("Your hand: " + playerDeckBuffer.toList.mkString(", ") + "\n")

        println("Your current score: " + calculateScore(playerDeckBuffer.toList))
        if (calculateScore(playerDeckBuffer.toList) > 21) {
          println("You busted! You lost!")
          playerDraw = false
          playerBUSS = true
        }
      } else if (upperCaseSelection == "S") { //stay
        playerDraw = false
      } else if (upperCaseSelection == "Q") { //quit
        playerDraw = false
        quit = true
      }
    }

    //keep drawing cards till 17
    if (!quit) {
      println("Dealer draws rest of cards.")
      while (calculateScore(dealerDeckBuffer.toList) < 17) {
        shuffledDeck = shuffledDeck.dropRight(1)

        dealerDeckBuffer += shuffledDeck.last
        shuffledDeck = shuffledDeck.dropRight(1)

        println("Dealers hand: " + dealerDeckBuffer.toList.mkString(", ") + "\n")
      }

      if (calculateScore(dealerDeckBuffer.toList) > 21) {
        dealerBUSS = true
        println("Dealer busts! You Win!")
      }

      if (dealerBUSS | playerBUSS) {
        if (dealerBUSS & playerBUSS) {
          println("What's wrong with y'all?")
        }
        else if (dealerBUSS) {
          println("The dealer busted")
        }
        else if (playerBUSS) {
          println("You busted")
        }
      } else {
        if (calculateScore(dealerDeckBuffer.toList) > calculateScore(playerDeckBuffer.toList)) {
          println("Dealer wins!")
        } else if (calculateScore(dealerDeckBuffer.toList) < calculateScore(playerDeckBuffer.toList)) {
          println("You Win!")
        } else {
          println("It's a tie!")
        }
      }
    }
    else {
      println("wOW WAy tO RaGe QuIt")
    }
  }
}
