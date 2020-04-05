package fundamental_algorithms

object DecimalToBinaryConvApp {
  def main(args: Array[String]): Unit = {
    println(decToBinConv(5))
    println(decToBinConv(8))
  }

  def decToBinConv(x: Int): String = {
    // Create a seqeence of numbers by dividing the given number by until it is less than 2.
    val seqOfDivByTwo = Iterator.iterate(x)(a => a / 2)
    println(seqOfDivByTwo)
    // Divid each number in sequence by 2 and stores the remainder in the collection binList, which is  an iterator of Int.
    val binList =  seqOfDivByTwo.takeWhile(a => a > 0).map(a => a % 2)
    binList.mkString.reverse
  }
}
