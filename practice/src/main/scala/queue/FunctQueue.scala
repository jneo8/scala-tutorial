package queue

case class FQueue(out: List[Int], in: List[Int]) {
  def check(): Boolean = (out, in) match {
    case (Nil, x :: xs) => false
    case _ => true
  }
  require(check, "Didn't satisfy invariant")
}

object FunctQueneApp{
  def main(args: Array[String]): Unit = {
    val myQueue = insert(15, insert(10, insert(5, FQueue(Nil, Nil))))
    println(remove(myQueue))
  }

  def insert(data: Int, queue: FQueue): FQueue = {
    val newIn = data :: queue.in
    queue.out match {
      case Nil => FQueue(newIn.reverse, Nil)
      case _ => queue.copy(in = newIn)
    }
  }

  def remove(queue: FQueue): (Int, FQueue) = {
    queue.out match {
      case Nil => throw new
        IllegalArgumentException("Queue is Empty")
      case x :: Nil => (x, queue.copy(out = queue.in.reverse, Nil))
      case y :: ys => (y, queue.copy(out = ys))
    }
  }
}
