import scala.util.Random
import scala.io.StdIn.readLine
import util.control.Breaks._
import scala.collection.mutable.ListBuffer

object Blackjack {

def calculateScore(cards: List[Any]) : Int = {
  var score : Int = 0
  var scoreInc: Int = 0
  var hasAce: Boolean = false

  for (i <- cards)
    {
      i match {
        case "2" => scoreInc = 2
        case "3" => scoreInc = 3
        case "4" => scoreInc = 4
        case "5" => scoreInc = 5
        case "6" => scoreInc = 6
        case "7" => scoreInc = 7
        case "8" => scoreInc = 8
        case "9" => scoreInc = 9
        case "10" => case "J" => case "Q" => case "K" => scoreInc = 10
        case "A" => hasAce = true
        case nil => println("card is nil")
      }

      if (hasAce)
        {
          if (score > 11)
            scoreInc = 1
          else
            scoreInc = 11
        }

      score += scoreInc
    }

  println("score: ", score)

  return score
}

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
    calculateScore(deck)

    val playerCards = List[String]()
    val dealerCards = List[String]()

    //Shuffle the deck randomly
    val shuffledDeck = Random.shuffle(deck)
    println(shuffledDeck)

    println("Dealer draws first card. \n")
    val dealerCardDeck = dealerCards ++ shuffledDeck.last
    val newDeck = shuffledDeck.dropRight(1)

    println("Player receives two cards")
    val y = playerCards ++ newDeck.last
    val newDeck2 = newDeck.dropRight(1)
    val z = playerCards ++ newDeck2.last
    val a = y ++ z
    val newDeck3 = newDeck2.dropRight(1)
    printStatus(a, dealerCardDeck)

    //println(calculateScore(z))

    var playerCardDeck = List[Any]()
    while(true) { // player decision loop
      println("Do you want to (H)it", "(S)tay, or (Q)uit")
      val selection = readLine()
      val upperCaseSelection = selection.toUpperCase()

        if (upperCaseSelection == "H") { //hit
          playerCardDeck = playerCards ++ newDeck3.last

          printStatus(playerCardDeck, dealerCardDeck)

          if (calculateScore(playerCardDeck) > 21) {
            println("You busted! You lost!")
            break
          }
        } else if (upperCaseSelection == "S") { //stay
          break
        } else if (upperCaseSelection == "Q") { //quit
          break
        }
      }


    println ("Dealer draws rest of cards.")
    //keep drawing cards till 17
    var deckBuffer = new ListBuffer[String]()
    while (calculateScore(dealerCardDeck) < 17) {
      val newDeck4 = newDeck3.dropRight(1)

      deckBuffer += newDeck4.last
      newDeck4.dropRight(1)
      println(newDeck4)
    }
    val toListDeck = deckBuffer.toList
    printStatus(playerCardDeck, toListDeck) // ***************

    if (calculateScore(toListDeck) > 21){
      println("Dealer busts! You Win!")
    } else if (calculateScore(toListDeck) > calculateScore(playerCardDeck)) {
      println("Dealer wins!")
    } else if (calculateScore(toListDeck) < calculateScore(playerCardDeck)) {
      println("You Win!")
    } else{
      println("It's a tie!")
    }
  }
}
