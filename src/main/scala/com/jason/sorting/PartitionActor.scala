package com.jason.sorting

/**
 * Created by Jason on 3/11/2015.
 */
import akka.actor.{Actor, ActorLogging, Props}

import scala.collection.mutable.ArrayBuffer

class PartitionActor extends Actor with ActorLogging {
  import com.jason.sorting.PartitionActor._
  log.info("New PartitionActor")


  def receive = {
    case msg:ListToPartition =>
      var less = ArrayBuffer.empty[Int]
      var equal = ArrayBuffer.empty[Int]
      var greater = ArrayBuffer.empty[Int]
      for (number:Int <- msg.list) {
        if (number < msg.pivot) less += number
        else if (number == msg.pivot) equal += number
        else greater += number
      }
      sender() ! SortActor.PartitionedList(less.toVector,equal.toVector,greater.toVector)
  }
}

object PartitionActor {
  val props = Props[PartitionActor]
  case class ListToPartition(list:Vector[Int], pivot:Int)
}
