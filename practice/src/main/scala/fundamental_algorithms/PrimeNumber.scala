package fundamental_algorithms

object PrimeNumbersApp {
  def main(args: Array[String]): Unit = {
    println(primes.take(10).toList)
  }

  val primes: Stream[Int] = 2 #:: Stream.from(3)
    .filter(
      x => {
        val sqrtOfPrimes = primes.takeWhile(y =>
            y <= math.sqrt(x)
        )
        !sqrtOfPrimes.exists(y => x % y == 0)
      }
    )
}
