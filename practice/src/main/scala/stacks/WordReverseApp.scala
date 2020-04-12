package stacks

class MyStackChar(maxSize: Int) {

  private var stackBox = new Array[Char](maxSize)
  private var top = -1

  def push(data: Char): Unit = {
    top += 1
    stackBox(top) = data
  }

  def pop(): Char = {
    val v = stackBox(top)
    top -= 1
    v
  }

  def peek(): Char = {
    stackBox(top)
  }

  def isEmpty(): Boolean = {
    return (top == -1)
  }

  def isFull(): Boolean = {
    return (top == maxSize - 1)
  }
}

class Reverser(word: String) {
  private val output: StringBuilder = new StringBuilder

  def reverse(): StringBuilder = {
    val myStack = new MyStackChar(word.length)

    for (eachChar <- word) {
      myStack.push(eachChar)
    }
    while(!myStack.isEmpty) {
      val poppedChar: Char = myStack.pop
      output.append(poppedChar)
    }
    output
  }
}


object WordReverseApp {
  def main(args: Array[String]): Unit = {
    print("Enter a word:")
    val inputWord = scala.io.StdIn.readLine().toString
    val myReverser = new Reverser(inputWord)

    println("Reverse word: " + myReverser.reverse)
  }
}
