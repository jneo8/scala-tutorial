package binary_tree

object BinTreeTraversal {
  def main(args: Array[String]): Unit = {
    val myList = List(1, 2, 3, 4, 5, 6)
    val myBinTree = createTree(myList)
    println(preorder(myBinTree))
    println(inorder(myBinTree))
    println(postorder(myBinTree))
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

  def createTree[A](list: List[A]): BinaryTree[A] = list match {
    case Nil => Leaf
    case x :: xs => {
      val halfLength = xs.length / 2
      Branch(
        x,
        createTree(xs.take(halfLength)),
        createTree(xs.drop(halfLength)),
      )
    }
  }
}
