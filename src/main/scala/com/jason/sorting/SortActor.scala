package com.jason.sorting

/**
 * Created by Jason on 3/11/2015.
 */
import akka.actor.{ActorRef, Actor, ActorLogging, Props}
import scala.util.Random

object SortActorType extends Enumeration {
  type SortActorType = Value
  val Less = Value
  val Greater = Value
  val DontCare = Value
}
import SortActorType._

class SortActor(level:Int, sortType:SortActorType) extends Actor with ActorLogging {
  import com.jason.sorting.SortActor._
  log.info("New SortActor")

  //actors
  var partitionActor:Option[ActorRef] = None  // context.actorOf(PartitionActor.props, "partitionActor")
  var sortChildLess:Option[ActorRef] = None  // context.actorOf(SortActor.props, "sortChildLess")
  var sortChildGreater:Option[ActorRef] = None  // context.actorOf(SortActor.props, "sortChildGreater")

  //variables
  var pivot:Option[Int] = None
  var less = Vector.empty[Int]
  var greater = Vector.empty[Int]
  var equal = Vector.empty[Int]
  var receivedLess = false
  var receivedGreater = false

  //recv cases
  def receive = {
    case msg:ListToSort =>
      log.info("From SortActor - got list " + msg.data.mkString(","))

      msg.data.length match {
        case 0 =>
          context.parent ! createSortedResultMessage
        case 1 =>
          equal = msg.data
          context.parent ! createSortedResultMessage
        case default =>
          //determines how many recursive levels to use, too many causes too much memory to be used
          if (level <= 0) {
            log.warning("max depth reached with list size of " + msg.data.size)
            equal = msg.data.sorted
            context.parent ! createSortedResultMessage
          //continue partitioning
          } else {
            partitionActor = Some(context.actorOf(PartitionActor.props, "partitionActor"))
            val index = Random.nextInt(msg.data.length)
            pivot = Some(msg.data(index))
            partitionActor.get ! PartitionActor.ListToPartition(msg.data, pivot.get)
          }
      }

    case msg:PartitionedList =>
      sortChildLess = Some(context.actorOf(SortActor.props(level-1,Less), "sortChildLess"))
      sortChildGreater = Some(context.actorOf(SortActor.props(level-1,Greater), "sortChildGreater"))
      sortChildLess.get ! SortActor.ListToSort(msg.less)
      sortChildGreater.get ! SortActor.ListToSort(msg.greater)
      equal = msg.equal

    case msg:SortedResultLess =>
      log.info("Received Less")
      less = msg.data
      receivedLess = true
      if (receivedGreater) {
        context.parent ! createSortedResultMessage
      }

    case msg:SortedResultGreater =>
      log.info("Received Greater")
      greater = msg.data
      receivedGreater = true
      if (receivedLess) {
        context.parent ! createSortedResultMessage
      }

  }

  def createSortedResultMessage(): SortedResultAbstract = {
      val sortedVector = less ++ equal ++ greater
      sortType match {
        case Less =>
          return SortedResultLess(sortedVector)
        case Greater =>
          return SortedResultGreater(sortedVector)
        case DontCare =>
          return SortedResult(sortedVector)
      }
  }

}

object SortActor {

  def props(level:Int, sortType:SortActorType = DontCare):Props = Props(new SortActor(level, sortType))
  case class ListToSort(data:Vector[Int])
  case class PartitionedList(less:Vector[Int], equal:Vector[Int], greater:Vector[Int])
  abstract class SortedResultAbstract{
    def data:Vector[Int]
  }
  case class SortedResult(data:Vector[Int]) extends SortedResultAbstract
  case class SortedResultLess(data:Vector[Int]) extends SortedResultAbstract
  case class SortedResultGreater(data:Vector[Int]) extends SortedResultAbstract


}
