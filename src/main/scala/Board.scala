/**
  * Created by dtch on 13/03/16.
  */
class Board(var squares: Array[Array[Piece]]) {
  if(squares == null)
    squares = Array(
      Array(null,null,null),
      Array(null,null,null),
      Array(null,null,null))

  def this(board: Board) = {
    this(board.squares.map(_.clone))
  }

  // Console printing for debugging purposes
  def print(): Unit = {
    Console.print("\n  0 1 2")
    for(i <- 0 until squares.length; j <- 0 until squares(i).length) {
      if(j==0) Console.print("\n" + i)
      if(squares(i)(j) != null && squares(i)(j) == Piece.Max) Console.print(Console.BOLD + Console.BLUE + " x" + Console.RESET)
      else if(squares(i)(j) != null && squares(i)(j) == Piece.Min) Console.print(Console.BOLD + Console.GREEN + " o" + Console.RESET)
      else Console.print(Console.BOLD + " ?" + Console.RESET)
    }
    Console.print("\n")
  }
}
