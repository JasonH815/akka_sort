package com.jason

import akka.actor.ActorSystem

object ApplicationMain extends App {
  val system = ActorSystem("AkkaSortSystem")
  val mainActor = system.actorOf(MainActor.props, "mainActor")
  val numbersToSort = 10000000
  val levels = 5 // there is a branching factor of 2, so total possible workers here is 2^levels
  mainActor ! MainActor.GO(numbersToSort,levels)
  system.awaitTermination()
}

