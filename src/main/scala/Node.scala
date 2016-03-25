/**
  * Created by dtch on 13/03/16.
  */
class Node(val board: Board, val max: Boolean) {
  var value: Int = 0
  var terminal: Boolean = false
  var children: Array[Node] = Array()

  checkTerminal()

  private def checkTerminal(): Unit = {
    // Control variables
    var emptyLines = 8
    var winner = false
    var piece: Piece = null

    // Do check
    for(i <- 0 until board.squares.length; j <- 0 until board.squares(i).length; if !winner && emptyLines > 0) {
      if((i == 0 || j == 0) && board.squares(i)(j) != null) {
        piece = board.squares(i)(j)
        if(i == 0) {
          // Check first row
          if(j == 0
            && board.squares(i)(j + 1) != null
            && board.squares(i)(j + 2) != null) {
            emptyLines -= 1
            if(piece == board.squares(i)(j + 1) && piece == board.squares(i)(j + 2))
              winner = true
          }
          // Check all col
          if(board.squares(i + 1)(j) != null
            && board.squares(i + 2)(j) != null) {
            emptyLines -= 1
            if (board.squares(i + 1)(j) == piece && board.squares(i + 2)(j) == piece)
              winner = true
          }
          // Check diag
          if(j == 0
            && board.squares(i + 1)(j + 1) != null
            && board.squares(i + 2)(j + 2) != null){
            emptyLines -= 1
            if (board.squares(i + 1)(j + 1) == piece && board.squares(i + 2)(j + 2) == piece)
              winner = true
          }
          // Check rev diag
          if(j == 2
            && board.squares(i + 1)(j - 1) != null
            && board.squares(i + 2)(j - 2) != null) {
            emptyLines -= 1
            if (board.squares(i + 1)(j - 1) == piece && board.squares(i + 2)(j - 2) == piece)
              winner = true
          }
        }
        else {
          // Check second and third row
          if(j == 0
            && board.squares(i)(j + 1) != null
            && board.squares(i)(j + 2) != null) {
            emptyLines -= 1
            if (board.squares(i)(j + 1) == piece && board.squares(i)(j + 2) == piece)
              winner = true
          }
        }
      }
    }

    // Assign results
    terminal = winner || emptyLines == 0
    if(winner) value = if(piece == Piece.Max) 1 else -1
  }

  private def boardMovements(): Array[Board] = {
    var boardMovements: Array[Board] = Array()
    for(i <- 0 until board.squares.length; j <- 0 until board.squares(i).length; if board.squares(i)(j) == null) {
      val boardMove = new Board(board)
      boardMove.squares(i)(j) = Piece.MINIMAX(max)
      boardMovements = boardMovements :+ boardMove
    }
    boardMovements
  }

  def buildTree(): Int = {
    if(!terminal) {
      var firstIteration = true
      for(boardMove <- boardMovements()) {
        val nodeChild = new Node(boardMove, !max)
        children = children :+ nodeChild
        val valueChild = nodeChild.buildTree()
        if(max && (value < valueChild) || !max && (value > valueChild) || firstIteration) {
          value = valueChild
          firstIteration = false
        }
      }
    }
    /*DEBUG board.print();println("");println("terminal:"+terminal);println("value:"+value);println("max:"+max)*/
    value
  }
}
