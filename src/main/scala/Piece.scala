/**
  * Created by dtch on 13/03/16.
  */

class Piece {}

object Piece {
  object Max extends Piece
  object Min extends Piece

  def MINIMAX(max: Boolean) = {
    if(max) Max
    else Min
  }
}

