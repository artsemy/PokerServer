package com.bootcamp
package room.game

import room.bean._
import room.bean.Suit._
import room.bean.Rank._

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import scala.util.Random
import scala.collection.mutable

object Main extends App {

    final class ServerActor extends Actor {

        private val roomActor: ActorRef = context.actorOf(Props[RoomActor](), "roomActor")
        private val playerActor: ActorRef = context.actorOf(Props[RoomActor](), "playerActor")
        private var flagFlop = true

        TestCommands()
        override def receive: Receive = {
            case "game started" => println("game started")
            case fl: Flop => println(fl)
            case "show flop" => flagFlop = false
        }

        def TestCommands(): Unit = {
            roomActor ! "open"
            roomActor ! "add player"
            roomActor ! "add player"
            //blind
            roomActor ! "small blind"
            roomActor ! "big blind"
            roomActor ! "set hands"
            //preflop
            roomActor ! "preflop" //fixxx
            roomActor ! "flop"
            while(flagFlop) {}
        }

    }

    final class GameActor extends Actor {

        private val roomActor: ActorRef = context.actorOf(Props[RoomActor](), "roomActor")
        private val players: mutable.Queue[Player] = mutable.Queue[Player]()

        override def receive: Receive = {
            case plrs: mutable.Queue[Player] => players.addAll(plrs)
        }

        def blind() = {
            roomActor
        }

    }

    final class PlayerActor extends Actor {

        override def receive: Receive = Actor.emptyBehavior

    }

    final class RoomActor extends Actor {

        private val gameActor: ActorRef = context.actorOf(Props[GameActor](), "gameActor")
        val playersToMakeBet: mutable.Queue[Player] = mutable.Queue()
        val playersMadeBet: mutable.Queue[Player] = mutable.Queue()
        private val board: Board = Board(null)
        private val pack = createPack()
        private val smallBlindPrice = 5
        private val bigBlindPrice = 10
        private var bank: Int = 0

        override def receive: Receive = {
            case "open" => println(self.path + " room opened")
            case "add player" => addPlayer()
            case "small blind" => makeSmallBlind()
            case "big blind" => makeBigBlined()
            case "set hands" => setHands()
            case "preflop" => makePreFlop()
            case "flop" => makeFlop()
        }

        private def makeFlop(): Unit = {
            val flop: Flop = Flop(pack.dequeue(), pack.dequeue(), pack.dequeue())
            sender() ! flop
            sender() ! "show flop"
        }

        private def makePreFlop(): Unit = { // fixxx
            // a lot of if's
            // a loop
            val player: Player = playersToMakeBet.dequeue()
            player.updateMoney(-smallBlindPrice)

        }

        private def setHands(): Unit = {
            playersToMakeBet.map(pl => pl.setHand(Hand(pack.dequeue(), pack.dequeue())))
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
                playersToMakeBet += Player(Random.nextLong()).updateMoney(100)
                println(self.path + " player added")
                if (playersToMakeBet.size == 2) {
                    gameActor ! playersToMakeBet
                    gameActor ! "start"
                    sender() ! "game started"
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
