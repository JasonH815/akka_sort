package com.jason

import akka.actor._
import com.jason.input.InputActor
import com.jason.sorting.{SortActorType, SortActor}

class MainActor extends Actor with ActorLogging {
  import MainActor._
  log.info("New MainActor")

  val inputActor = context.actorOf(InputActor.props, "inputActor")
  var sortActor:Option[ActorRef] = None  // context.actorOf(SortActor.props(SortActorType.DontCare), "sortActor")
  var numbers = 0
  var levels = 100




  def receive = {
    case msg:GO =>
      numbers = msg.numbers
      levels = msg.levels
      inputActor ! InputActor.Initialize(numbers)
    case msg:UserInput =>
      log.info("Got user input " + msg.input.mkString(","))
      sortActor = Some(context.actorOf(SortActor.props(levels, SortActorType.DontCare), "sortActor"))
      sortActor.get ! SortActor.ListToSort(msg.input)
    case msg:SortActor.SortedResult =>
      if (msg.data.size > 0) {
        var x = msg.data(0)
        var i = 1;
        while (i < msg.data.size && x <= msg.data(i)) {
          x = msg.data(i)
          i += 1
        }
        val result:String = if (i < msg.data.size) "not sorted" else "sorted"
        println("\n\n The Final result is " + result + ". with size " + msg.data.size)
      }
      context.parent ! PoisonPill
  }
}

object MainActor {
  val props = Props[MainActor]
  case class GO(numbers:Int, levels:Int)
  case class UserInput(input:Vector[Int])
}