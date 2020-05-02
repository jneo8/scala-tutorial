package searching

object KnuthMorrisPrattApp {
  def main(args: Array[String]): Unit = {
    val myDatal = "This is a functional implementation."
    val myWord1 = "functional"
    println(kmpSubStringSearch(myWord1, myDatal))
  }

  def kmpSubStringSearch(myWords: String, myData: String): Int = {
    val prefixTab = prefixTable(myWords)

    myData.indices.foldLeft(-1, 0) {
      case ((foundIndex, x), i) if foundIndex > 0 => (foundIndex, 0)
      case ((foundIndex, x), i) => {
        val stepx = Stream.iterate(x)(x => prefixTab(x - 1))
        val lowerx = stepx.find(x => x == 0 || myWords(x) == myData(i)).get
        val newX = if (myWords(lowerx) == myData(i)) lowerx + 1 else lowerx
        if(newX == myWords.length)(i - newX + 1, 0) else (-1, newX)
      }
    }._1
  }

  def prefixTable(searchString: String): Vector[Int] = {
    searchString.drop(1).foldLeft(0, Vector(0)) {
      case ((initialValue, prefixT), currentCharacter) => {
        val lowerValue = Stream.iterate(initialValue)(
          initialValue => prefixT(initialValue - 1))
            .find(initialValue => initialValue == 0 ||
              searchString(initialValue) == currentCharacter)
            .get
            val newValue = if (searchString(lowerValue) ==
              currentCharacter) lowerValue + 1 else lowerValue
            (newValue, prefixT :+ newValue)
      }
    }._2
  }
}
