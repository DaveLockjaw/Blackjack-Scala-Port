import scala.util.Random
import scala.io.StdIn.readLine
import util.control.Breaks._

object Blackjack {

  //def calculateScore(cards: List[Any]) : Int = {
  //  var score : Int = 0
  //  var hasAce = false
//
  //
//
  //}

  def printStatus(playerCards: List[Any], dealerCards: List[Any]): Unit = {
    println("")
    println("Player's total is " + playerCards)

    playerCards.foreach {println}
    println("")

    println("Dealer's total is " + dealerCards)
    dealerCards.foreach {println}
    println("")
  }

  def main(args: Array[String]) = {
    val deck = List[String]("2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A")
    val playerCards = List[String]()
    val dealerCards = List[String]()

    //Shuffle the deck randomly
    val shuffledDeck = Random.shuffle(deck)
    println(shuffledDeck)

    println("Dealer draws first card. \n")
    val x = dealerCards ++ shuffledDeck.last
    val newDeck = shuffledDeck.dropRight(1)

    println("Player receives two cards")
    val y = playerCards ++ newDeck.last
    val newDeck2 = newDeck.dropRight(1)
    val z = playerCards ++ newDeck2.last
    val a = y ++ z
    val newDeck3 = newDeck2.dropRight(1)
    printStatus(a, x)

    while(true) { // player decision loop
      println("Do you want to (H)it", "(S)tay, or (Q)uit")
      val selection = readLine()
      val upperCaseSelection = selection.toUpperCase()

      if (upperCaseSelection == "H") { //hit
        val b = playerCards ++ newDeck3.last
        val newDeck4 = newDeck3.dropRight(1)
        printStatus(b, x)

        if (calculateScore(b) > 21) {
          println("You busted! You lost!")
          return 0
        }
      } else if (upperCaseSelection == "S") { //stay
        break
      } else if (upperCaseSelection == "Q") { //quit
        return 0
      }
    }

    println ("Dealer draws rest of cards.")
    //keep drawing cards till 17
    while (calculateScore(x) > 21) {

    }
    printStatus(b, x)
  }
}
