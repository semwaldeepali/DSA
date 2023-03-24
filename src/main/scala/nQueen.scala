import scala.math.abs

object nQueen {
  def checkDiagonal(col: Int, dist: Int, board: List[Int]) : Boolean = board match
    case Nil => true
    case c::others =>
      if abs(c-col) == dist then false
      else checkDiagonal(col, dist + 1, others)

  def isSafe(col: Int, board: List[Int]) : Boolean =
    //check for valid placement : vertically and diagonally
    //Vertically : loop over board to see if col matches
    if board.exists(_ == col) then false
    else
      checkDiagonal(col,1,board)


  def placeQueenHelper(n : Int, k : Int) : Set[List[Int]] =
    if k == 0 then Set(List())
    else
      // for each possible placement in the 0 to k -1 rows of k queens
      // boardSoFar contains column in which queens are placed. last element in the list corresponds to 1st row and so on.
       for boardSoFar <- placeQueenHelper(n, k - 1)
       // for all possible column values for kth row
       col <- 0 until n
       //if it is safe to place the queen at particular col, add and return
        if isSafe(col, boardSoFar)
      yield col :: boardSoFar


  def placeNQueen(k : Int) : Set[List[Int]] =
    placeQueenHelper(k, k)

  def main(args:Array[String]) : Unit =
    println(placeNQueen(4))
    println(placeNQueen(2))
    println(placeNQueen(0))
    println(placeNQueen(1))

}
