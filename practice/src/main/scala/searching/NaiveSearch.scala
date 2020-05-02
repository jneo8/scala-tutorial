package searching

object NaiveSubStringSearchApp {
  def main(args: Array[String]): Unit = {
    val myDatal = "This is a functional implementation."
    val myWord1 = "functional"
    println(naiveSubStringSearch(myWord1, myDatal))
  }

  def naiveSubStringSearch(myWords: String, myData: String): Int = {
    myData.indices.find{
      i => i + myWords.length <= myData.length && myWords.indices.forall(j => myData(j + i) == myWords(j))}.getOrElse(-1)
  }
}
