package generics

import scala.collection.mutable.ArrayBuffer

class Queue[T] {

  private var queue = new ArrayBuffer[T]
  var size = 0

  def enqueue(e: T) {
    queue += e
    size += 1
  }

  def dequeue(): T = {
    if (size == 0) throw new Exception("Empty queue")
    queue.remove(0)
  }

  def isEmpty() = {
    if (queue.isEmpty) {
      true
    } else false
  }

  override def toString(): String = {
    queue.toString()
  }
}

object Queue {
  def apply = new Queue

}

object Test extends App {
  var waitingList = new Queue[String]
  waitingList.enqueue("Anjali")
  waitingList.enqueue("Priyanka")
  waitingList.enqueue("Deepika")
  waitingList.enqueue("Anushka")
  waitingList.enqueue("Alia")

  while (!waitingList.isEmpty()) {
    println(waitingList.dequeue().toString())
  }
}

//> OUTPUT:
//Anjali
//Priyanka
//Deepika
//Anushka
//Alia