package array


object MatrixMultApp {
  def main(args: Array[String]) : Unit = {
    val myMatrix1: Array[Array[Double]] = Array(
      Array(2.5, 1.5, 0.5),
      Array(1, 2, 4)
    )
    val myMatrix2: Array[Array[Double]] = Array(
      Array(-1, -1.5, 1, -1),
      Array(0.5, -2, -2.5, 1),
      Array(1, 2, 1, 1)
    )

    val myResultMatrix: Array[Array[Double]] = multiplyMatrices(myMatrix1, myMatrix2)
    printMatrix(myResultMatrix)
  }

  def printMatrix(matrix: Array[Array[Double]]) {
    matrix foreach {
      row => row.zipWithIndex foreach {
        case(v, idx) => {
          print(v);
          if( idx != row.length - 1){
            print(",")
          }
        }
      };
      println
    }
  }

  def multiplyMatrices(mat1: Array[Array[Double]], mat2: Array[Array[Double]]): Array[Array[Double]] = {

    var resultMatrix = Array.ofDim[Double](mat1.length, mat2(0).length)

    val mat1Rows = mat1.length
    val mat1Columns = mat1(0).length
    val mat2Rows = mat2.length
    val mat2Columns = mat2(0).length

    if (mat1Columns != mat2Rows) {
      println(f"Mat1Columns: $mat1Columns%d != Mat2Rows: $mat2Rows%d")
    } else {
      println(f"Mat1Columns: $mat1Columns%d == Mat2Rows: $mat2Rows%d")
      for (i <- 0 until mat1Rows) {
        for (j <- 0 until mat2Columns) {
          for (k <- 0 until mat1Columns) {
            resultMatrix(i)(j) += mat1(i)(k) * mat2(k)(j)
          }
        }
      }
    }
    resultMatrix
  }
}


