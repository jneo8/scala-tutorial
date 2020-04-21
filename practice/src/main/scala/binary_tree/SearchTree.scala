package binary_tree

object BinarySearchTreeApp {
  type Dictionary[A] = BinaryTree[(String, A)]

  def empty[A](): Dictionary[A] = Leaf

  def main(args: Array[String]): Unit = {
    val myWordList = List(
      ("cat", 5),
      ("dog", 7),
      ("the", 12),
      ("for", 4),
      ("then", 11),
    )

    val myBinSearchTree = myWordList.foldLeft(empty[Int]())((y, x) => insert(x._1, x._2, y))

    println(preorder(myBinSearchTree))
    println(inorder(myBinSearchTree))
    println(postorder(myBinSearchTree))
    println(searchKey("for", myBinSearchTree))
  }

  def insert[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, v), lb, rb) if (key == k) => sys.error(s"key ${key} already present")
    case Branch((k, v), lb, rb) if (key < k) => Branch((k, v), insert(key, value, lb), rb)
    case Branch((k, v), lb, rb) if (key > k) => Branch((k, v), lb, insert(key, value, rb))
  }

  def searchKey[A](key: String, dict: Dictionary[A]): Option[A] = dict match {
    case Leaf => None
    case Branch((k, v), lb, rb) if (key == k) => Some(v)
    case Branch((k, v), lb, rb) if (key < k) => searchKey(key, lb)
    case Branch((k, v), lb, rb) if (key > k) => searchKey(key, rb)
  }

  def updateValue[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, v), lb, rb) if (key == k) =>
      Branch((key, value), lb, rb)
    case Branch((k, v), lb, rb) if (key < k) =>
      Branch((key, value), updateValue(key, value, lb), rb)
    case Branch((k, v), lb, rb) if (key > k) =>
      Branch((key, value), lb, updateValue(key, value, rb))
  }

  def preorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) =>
      value :: (preorder(leftBranch) ++ preorder(rightBranch))
  }

  def inorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) =>
      inorder(leftBranch) ++ (value :: inorder(rightBranch))
  }

  def postorder[A](binTree: BinaryTree[A]): List[A] = binTree match {
    case Leaf => Nil
    case Branch(value, leftBranch, rightBranch) =>
      postorder(leftBranch) ++ postorder(rightBranch) ++ List(value)
  }
}
