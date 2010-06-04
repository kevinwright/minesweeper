package lsug.scaladojo.minesweeper

object MatrixOps {
  type Matrix[T] = Seq[Seq[T]]
  type SeqFunc[A,B] = Seq[A] => Seq[B]
  implicit def matrixToMatrixOps[T](matrix : Matrix[T]) = new MatrixOps(matrix)
}

import MatrixOps._

class MatrixOps[T](matrix : Matrix[T]) {
  //apply the supplied mapping to each cell
  def mapCells[R](f : T => R) = matrix map {_ map f}

  //map only the row as the supplied index
  def mapRow(row : Int)(f : SeqFunc[T,T]) : Matrix[T] = {
    val (top, bottom) = matrix.splitAt(row)
    top ++ (f(bottom.head) +: bottom.tail)
  }

  //map only the cell as the supplied co-ordinates
  def mapCell(col : Int, row : Int)(f : T => T) : Matrix[T] = {
    def rowFunc(row : Seq[T]) : Seq[T] = {
      val (left, right) = row.splitAt(col)
      left ++ (f(right.head) +: right.tail)
    }
    mapRow(row)(rowFunc)
  }

  //apply the mapping function to each row
  def mapRows[R](f: SeqFunc[T,R]) = matrix.map(f)

  //apply the mapping function to each column
  def mapCols[R](f: SeqFunc[T,R]) = matrix.transpose.map(f).transpose

  //apply the mapping function to each row, then to each column of the resulting grid
  def map2d(f: SeqFunc[T,T]) = mapRows(f).mapCols(f)

  def count (f: T => Boolean) = matrix.map(_.count(f)).sum

}
