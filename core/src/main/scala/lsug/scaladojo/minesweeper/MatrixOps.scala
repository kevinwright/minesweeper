package lsug.scaladojo.minesweeper

object MatrixOps {
  type Matrix[T] = Seq[Seq[T]]
  type SeqFunc[A,B] = Seq[A] => Seq[B]
  implicit def matrixToMatrixOps[T](matrix : Matrix[T]) = new MatrixOps(matrix)
}

import MatrixOps._

class MatrixOps[A](matrix : Matrix[A]) {
  //apply the supplied mapping to each cell
  def mapCells[B](f : A => B) = matrix map {_ map f}

  //map only the row as the supplied index
  def mapRow(row : Int)(f : SeqFunc[A,A]) : Matrix[A] = {
    val (top, bottom) = matrix.splitAt(row)
    top ++ (f(bottom.head) +: bottom.tail)
  }

  //map only the cell as the supplied co-ordinates
  def at(col : Int, row : Int)(f : A => A) : Matrix[A] = {
    def rowFunc(row : Seq[A]) : Seq[A] = {
      val (left, right) = row.splitAt(col)
      left ++ (f(right.head) +: right.tail)
    }
    mapRow(row)(rowFunc)
  }

  //apply the mapping function to each row
  def mapRows[B](f: SeqFunc[A,B]) = matrix.map(f)

  //apply the mapping function to each column
  def mapCols[B](f: SeqFunc[A,B]) = matrix.transpose.map(f).transpose

  //apply the mapping function to each row, then to each column of the resulting grid
  def map2d(f: SeqFunc[A,A]) = mapRows(f).mapCols(f)

  def count (f: A => Boolean) = matrix.map(_.count(f)).sum

  def cellExists (f: A => Boolean) = matrix.exists(row => row.exists(f))

  def deepZip[B](other : Matrix[B]) : Matrix[(A,B)] = {
    (matrix, other).zipped map (_ zip _)
  }

  def delta(next : Matrix[A]) : Matrix[Option[A]] =
    (matrix deepZip next) mapCells {
      case (a,b) if a==b => None
      case (_,b) => Some(b)
    }

  def coords : Matrix[(Int, Int)] =
    matrix.zipWithIndex.map {
      case(row, y) => row.indices.map( x => (x, y) )
    }

  def zipWithCoords : Matrix[(Int, Int, A)] =
    (coords deepZip matrix) mapCells {
      case ((x,y),c) => (x, y, c)
    }

  def matrixToString =
    mapCells(_.toString).mapRows(_.mkString(" ")).mkString("\n")
}
