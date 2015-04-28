package com.jason.input

/**
 * Created by Jason on 3/11/2015.
 */
import akka.actor.{Actor, ActorLogging, Props}
import com.jason.MainActor

import scala.util.Random

class InputActor extends Actor with ActorLogging {
  import com.jason.input.InputActor._
  log.info("New InputActor")

  var inputList = scala.collection.mutable.ArrayBuffer.empty[Int]

  def receive = {
    case msg:Initialize =>
      log.info("In InputActor - getting user defined list...")
      context.parent ! generateRandomInput(msg.numbers)
  }

  def processInput():MainActor.UserInput = {
    var input:String = "q"

    input = scala.io.StdIn.readLine("Enter an integer value or 'q' to stop: ")
    while (!input.equals("q")) {

      //parse int
      try {
        val inputNumber = Integer.parseInt(input)
        log.info("user input " + inputNumber)
        inputList += inputNumber
      }
      //catch possible number format exception
      catch {
        case _: NumberFormatException =>
          println("Invalid number.")
          log.info("invalid user input")
      }

      //get next input
      input = scala.io.StdIn.readLine("Enter an integer value or 'q' to stop: ")
    }
    return MainActor.UserInput(inputList.toVector)

  }

  def generateRandomInput(n:Int):MainActor.UserInput = {
    for (x <- 0 until n) inputList += Random.nextInt()
    MainActor.UserInput(inputList.toVector)
  }


}

object InputActor {
  val props = Props[InputActor]
  case class Initialize(numbers:Int)
}
