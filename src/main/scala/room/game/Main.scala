package com.bootcamp
package room.game

import room.bean._
import room.bean.Message._
import room.actor.ServerActor

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

object Main extends App {

    final class ClientActor extends Actor {
        private val serverActor: ActorRef = context.actorOf(Props[ServerActor](), "serverActor")

        override def receive: Receive = {
            Actor.emptyBehavior // add printing result
        }

        def TestCommands(): Unit = { //testing case
            val player1: Player = Player(1)
            val player2: Player = Player(2)
            val player3: Player = Player(3)
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
            // flop dealing -> flop round
            serverActor ! mMakeBet(player2, 20)
            serverActor ! mMakeBet(player3, 20)
            serverActor ! mMakeBet(player1, 20)
            // turn dealing -> turn round
            serverActor ! mMakeBet(player2, 0)
            serverActor ! mMakeBet(player3, 0)
            serverActor ! mMakeBet(player1, 0)
            // river dealing -> river round
            serverActor ! mMakeBet(player2, 10)
            serverActor ! mMakeBet(player3, 10)
            serverActor ! mMakeBet(player1, 10)
        }

        TestCommands()
    }

    val evoActorSystem: ActorSystem = ActorSystem("poker-actor-system")
    evoActorSystem.actorOf(Props[ClientActor]())

}
