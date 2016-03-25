object TicTacToe {
  def main(args: Array[String]): Unit = {

    val initialBoardSquares: Array[Array[Piece]] = Array(
      Array(null,null,null),
      Array(null,null,null),
      Array(null,null,null)
    )

    var root = new Node(
      new Board(initialBoardSquares),
      false
    )

    do{
      if(root.max) root = makeMaxMove(root)
      else root = changeRoot(root, readMinMove(root.board))
    }while(!root.terminal)

    // End game console printing for debugging purposes
    root.board.print()
    Console.print("Fin. ")
    if(root.value > 0) Console.println ("El programa gana.")
    else if(root.value < 0) Console.println ("El usuario gana.")
    else Console.println ("Empate.")
  }

  def makeMaxMove(node: Node): Node = {
    var newNode: Node = null
    for(i <- node.children; if newNode == null || newNode.value <= i.value) newNode = i
    newNode
  }

  // Console read for debugging purposes
  def readMinMove(board: Board): Board = {
    var move: String = null

    board.print()
    do
      move = readLine("Escriba posición nuevo movimiento (columnrow):")
    while(board.squares(move.charAt(1).asDigit)(move.charAt(0).asDigit) != null)

    val newBoard = new Board(board)

    newBoard.squares(move.charAt(1).asDigit)(move.charAt(0).asDigit) = Piece.Min

    newBoard
  }

  def changeRoot(oldRoot: Node, newBoard: Board): Node = {
    var newRoot: Node = null

    if(!oldRoot.children.isEmpty) {
      for(i <- oldRoot.children; if i.board.squares.deep == newBoard.squares.deep) newRoot = i
    }

    if(newRoot == null)  {
      newRoot = new Node(
        new Board(newBoard),
        !oldRoot.max
      )
      newRoot.buildTree()
    }

    newRoot
  }
}


