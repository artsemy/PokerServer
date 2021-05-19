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

    final class ServerActor extends Actor {

        private val roomActor: ActorRef = context.actorOf(Props[RoomActor](), "roomActor")
        private var flagFlop = true


        override def receive: Receive = {
            case fl: Flop => println(fl)
        }

        TestCommands()

        def TestCommands(): Unit = {
            roomActor ! mOpenRoom()
            roomActor ! mAddPlayer()
            roomActor ! mAddPlayer()
            //blind
            roomActor ! mSmallBlind()
            roomActor ! mBigBlind()
            roomActor ! mSetHands()
            //preflop
            roomActor ! mPreFlop() //fixxx
            roomActor ! mMakeFlop()
        }

    }

    final class RoomActor extends Actor {

        val playersToMakeBet: mutable.Queue[Player] = mutable.Queue()
        val playersMadeBet: mutable.Queue[Player] = mutable.Queue()
        private val board: Board = Board(null)
        private val pack = createPack()
        private val smallBlindPrice = 5
        private val bigBlindPrice = 10
        private var bank: Int = 0

        override def receive: Receive = {
            case m: mOpenRoom => println(self.path + " room opened")
            case m: mAddPlayer => addPlayer()
            case m: mSmallBlind => makeSmallBlind()
            case m: mBigBlind => makeBigBlined()
            case m: mSetHands => setHands()
            case m: mPreFlop => makePreFlop()
            case m: mMakeFlop => makeFlop()
        }

        private def makeFlop(): Unit = {
            val flop = Flop(pack.dequeue(), pack.dequeue(), pack.dequeue())
            sender ! flop
        }

        private def makePreFlop(): Unit = { // fixxx
            // a lot of if's
            // a loop
            val player: Player = playersToMakeBet.dequeue()
            player.updateMoney(-smallBlindPrice)

        }

        private def setHands(): Unit = {
            playersToMakeBet.foreach(pl => pl.setHand(Hand(pack.dequeue(), pack.dequeue())))
        }

        private def makeSmallBlind(): Unit = {
            val player: Player = playersToMakeBet.dequeue()
            player.updateMoney(-smallBlindPrice)
            bank += smallBlindPrice
            playersMadeBet.append(player)
            // two players
            playersToMakeBet.appendAll(playersMadeBet)
            playersMadeBet.clear()
        }

        private def makeBigBlined(): Unit = {
            val player: Player = playersToMakeBet.dequeue()
            player.updateMoney(-bigBlindPrice)
            bank += bigBlindPrice
            playersMadeBet.append(player)
        }

        private def addPlayer(): Unit = {
            if (playersToMakeBet.size != 2) {
              val pl = Player(Random.nextLong())
              pl.updateMoney(100)
              playersToMakeBet.append(pl)
              println(self.path + " player added")
              if (playersToMakeBet.size == 2) {
                  //new actor
              }
            }
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
    evoActorSystem.actorOf(Props[ServerActor]())

}
