package com.bootcamp
package room.game

import room.bean._
import room.bean.Suit._
import room.bean.Rank._
import room.bean.Message._

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.util.Random
import scala.collection.mutable

object Main extends App {

    final class ClientActor extends Actor {
        private val serverActor: ActorRef = context.actorOf(Props[ServerActor](), "serverActor")

        override def receive: Receive = {
            Actor.emptyBehavior // print result
        }

        def TestCommands(): Unit = { //testing case
            val player1: Player = Player(Random.nextLong())
            val player2: Player = Player(Random.nextLong())
            val player3: Player = Player(Random.nextLong())
            player1.updateMoney(100)
            player2.updateMoney(100)
            player3.updateMoney(100)
            serverActor ! mOpenRoom()
            serverActor ! mAddPlayer(player1)
            serverActor ! mAddPlayer(player2)
            serverActor ! mAddPlayer(player3)
            serverActor ! mStartGame()
            // blind -> dealingCards -> preFlop round
            serverActor ! mMakeBet(player3, 10)
            serverActor ! mMakeBet(player1, 5)
            serverActor ! mMakeBet(player2, 0)
            // flop dealing -> flop round
            serverActor ! mMakeBet(player1, 20)
            serverActor ! mMakeBet(player2, 20)
            serverActor ! mMakeBet(player3, 20)
            // turn dealing -> turn round
            serverActor ! mMakeBet(player1, 10)
            serverActor ! mMakeBet(player2, 10)
            serverActor ! mMakeBet(player3, 10)
            // river dealing -> river round
            serverActor ! mMakeBet(player1, 10)
            serverActor ! mMakeBet(player2, 10)
            serverActor ! mMakeBet(player3, 10)
        }

        TestCommands()
    }

    final class ServerActor extends Actor {

        private val roomActor: ActorRef = context.actorOf(Props[RoomActor](), "roomActor")

        override def receive: Receive = {
            case m: mOpenRoom => openRoom()
            case m: mAddPlayer => addPlayer(m.player)
            case m: mStartGame => startGame()
            case m: mMakeBet => makeBet(m.player, m.bet)
        }

        private def openRoom(): Unit = roomActor ! mOpenRoom()
        private def addPlayer(player: Player): Unit = roomActor ! mAddPlayer(player)
        private def startGame(): Unit = {
            roomActor ! mSmallBlind()
            roomActor ! mBigBlind()
            roomActor ! mSetHands()
        }
        private def makeBet(pl: Player, bet: Int): Unit = roomActor ! mRoundStep(bet)

    }

    final class RoomActor extends Actor {

        private val playersActive: mutable.Queue[Player] = mutable.Queue() //next round players
        private val playersAll: mutable.Queue[Player] = mutable.Queue() //next game players
        private val pack = createPack()
        private val smallBlindPrice = 5
        private val bigBlindPrice = 10
        private var bank: Int = 0
        private var roundBet: Int = 0
        private var canCheck: Int = 0
        private var boardSize: Int = 0

        override def receive: Receive = {
            case m: mOpenRoom => init()
            case m: mAddPlayer => addPlayer(m.player)
            case m: mSmallBlind => makeSmallBlind()
            case m: mBigBlind => makeBigBlind()
            case m: mSetHands => setHands()
            case m: mRoundStep => makeRoundStep(m.bet)
        }

        private def makeRoundStep(pBet: Int): Unit = {
            if (boardSize == 0) preFlopRound(pBet)
            else afterFlopRound(pBet)
        }

        private def afterFlopRound(pBet: Int): Unit = {
            if (pBet < 0) afterFlopRoundFold()
            if (pBet == 0) afterFlopRoundCheck()
            if (pBet > 0) afterFlopRoundRise(pBet)
        }

        private def afterFlopRoundRise(pBet: Int): Unit = {
            val player = playersActive.dequeue()
            player.updateMoney(-pBet)
            roundBet += pBet
            bank += pBet
            player.setCurrentBet(roundBet)
            playersActive.append(player)
            canCheck = 0
        }

        private def afterFlopRoundFold(): Unit = playersActive.dequeue()

        private def afterFlopRoundCheck(): Unit = {
            val player = playersActive.dequeue()
            playersActive.append(player)
            canCheck += 1
            if (canCheck == playersActive.length - 1) closeRound()
        }

        private def preFlopRound(pBet: Int): Unit = { //works only with correct input
            val player = playersActive(0)
            if (pBet < 0) preFlopRoundFold()
            if (pBet + player.getCurrentBet() == roundBet) preFlopRoundCheck(pBet)
            if (pBet + player.getCurrentBet() > roundBet) preFlopRoundRaise(pBet)
        }

        private def preFlopRoundRaise(pBet: Int): Unit = {
            val player = playersActive.dequeue()
            player.updateMoney(-pBet)
            roundBet = pBet + player.getCurrentBet()
            bank += pBet
            player.setCurrentBet(roundBet)
            playersActive.append(player)
        }

        private def preFlopRoundCheck(pBet: Int): Unit = {
            if (pBet == 0) closeRound()
            else { // small-blind
                val player = playersActive.dequeue()
                player.updateMoney(-pBet)
                player.setCurrentBet(roundBet)
                bank += pBet
                playersActive.append(player)
            }
        }

        private def closeRound(): Unit = {
            canCheck = 0
            roundBet = 0
            playersActive.foreach(pl => pl.setCurrentBet(0))
            if (boardSize == 0) makeFlop()
            if (boardSize == 3) makeTurn()
            if (boardSize == 4) makeRiver()
        }

        private def makeRiver(): Unit = {
            val card = pack.dequeue()
            boardSize = 5 //add "adding cards to board"
            println(card)
        }

        private def makeTurn(): Unit = {
            val card = pack.dequeue()
            boardSize = 4 //add "adding cards to board"
            println(card)
        }

        private def makeFlop(): Unit = {
            val flop = Flop(pack.dequeue(), pack.dequeue(), pack.dequeue())
            boardSize = 3 //add "adding cards to board"
            println(flop) // log
        }

        private def preFlopRoundFold(): Unit = playersActive.dequeue()

        private def setHands(): Unit = {
            playersActive.foreach(pl => pl.setHand(Hand(pack.dequeue(), pack.dequeue())))
            playersActive.foreach(pl => println(pl.getHand())) // log
            prepareFlopRound()
        }

        private def prepareFlopRound(): Unit = {
            val player1 = playersActive.dequeue()
            playersActive.append(player1)
            val player2 = playersActive.dequeue()
            playersActive.append(player2)
            roundBet = bigBlindPrice
        }

        private def makeBigBlind(): Unit = {
            val player: Player = playersActive(1)
            player.updateMoney(-bigBlindPrice)
            bank += bigBlindPrice
            player.setCurrentBet(bigBlindPrice)
        }

        private def makeSmallBlind(): Unit = {
            val player: Player = playersActive(0)
            player.updateMoney(-smallBlindPrice)
            bank += smallBlindPrice
            player.setCurrentBet(smallBlindPrice)
        }

        private def addPlayer(player: Player): Unit = {
            playersActive.append(player)
            playersAll.append(player)
        }

        private def init(): Unit = { //init something
            println("game start")
        }

        private def createPack(): mutable.Queue[Card] = {
            val cards: mutable.Queue[Card] = mutable.Queue()
            val suits = List(Hearts, Diamonds, Spades, Clubs)
            val ranks = List(Two(), Three(), Four(), Five(), Six(), Seven(),
                Eight(), Nine(), Ten(), Jack(), Queen(), King(), Ace())
            for (r <- ranks) {
                for (s <- suits) {
                    cards += Card(r, s)
                }
            }
            Random.shuffle(cards)
        }
    }

    val evoActorSystem: ActorSystem = ActorSystem("poker-actor-system")
    evoActorSystem.actorOf(Props[ClientActor]())

}
