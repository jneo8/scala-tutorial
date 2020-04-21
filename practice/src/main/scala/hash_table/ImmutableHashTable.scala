package hash_table

trait ImmutableHashTable[Key, Value] {
  def insert(myKey: Key, myValue: Value): ImmutableHashTable[Key, Value]
  def search(myKey: Key): Option[Value]
  def delete(myKey: Key): ImmutableHashTable[Key, Value]
}


protected class HashTableImmutableImpl[Key, Value] (
  myHashVector: Vector[List[(Key, Value)]]
) extends ImmutableHashTable[Key, Value] {
  private val size = myHashVector.size

  def hashCode[Key](myKey: Key) = {
    val tempHashCode = myKey.## % size
    if (tempHashCode < 0) tempHashCode + size else tempHashCode
  }

  override def insert(myKey: Key, myValue: Value) = {
    val insertionIndex = hashCode(myKey)
    val insertionList = myHashVector(insertionIndex)
    val newList = (myKey, myValue) +: insertionList.filter(_._1 != myKey)
    new HashTableImmutableImpl[Key, Value](myHashVector.updated(insertionIndex, newList))
  }

  override def search(myKey: Key): Option[Value] = {
    val myList = myHashVector(hashCode(myKey))
    myList.find(x => x._1 == myKey).map(y => y._2)
  }

  override def delete(myKey: Key) = {
    val deletionIndex = hashCode(myKey)
    val deletionList = myHashVector(deletionIndex)
    val newList = deletionList.filter(_._1 != myKey)
    new HashTableImmutableImpl[Key, Value](myHashVector.updated(deletionIndex, newList))
  }
}

object HashTableImmutableImpl {
  def apply[Key, Value](size: Int) = {
    val myHashVector = Vector.fill(size)(List())
    new HashTableImmutableImpl[Key, Value](myHashVector)
  }
}

object HashTableImmutableApp {
  def main(args: Array[String]): Unit = {
    val myHashTable: ImmutableHashTable[Int, String] = HashTableImmutableImpl(17)

    val filledTable = myHashTable
      .insert(123456789, "Martin")
      .insert(987654321, "James")
      .insert(123454321, "Brain")
      .insert(432112345, "Einstein")
      .insert(776612345, "Richie")

    println(s" Martin search, ${filledTable.search(123456789)}")
    println(s" James search, ${filledTable.search(987654321)}")
    println(s" Brain search, ${filledTable.search(123454321)}")
    println(s" Einstein search, ${filledTable.search(432112345)}")
    println(s" Richie search, ${filledTable.search(776612345)}")

    val removeRichie = filledTable.delete(776612345)
    val notExisting = filledTable.delete(886612345)

    println(s" Richie search, ${removeRichie.search(776612345)}")
    println(s"Non-existing search, ${notExisting.search(886612345)}")
    println(s"Richie search in original, ${filledTable.search(776612345)}")
  }
}
