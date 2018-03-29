class Board(var board: List[String]) {
	def print(){
		println(board(0) + " " + board(1) + " " + board(2))
		println(board(3) + " " + board(4) + " " + board(5))
		println(board(6) + " " + board(7) + " " + board(8))
	}

	def check(){
		var winner = ""
		List("X","O").foreach { p =>
			if (hasSame(p,0,1,2) || hasSame(p,3,4,5) || hasSame(p,6,7,8) ||
				hasSame(p,0,3,6) || hasSame(p,1,4,7) || hasSame(p,2,5,8) ||
				hasSame(p,1,4,8) || hasSame(p,2,4,6)){
				winner = p
			}
		}

		if (winner != ""){
			println(winner + " has won")
		} else {
			println("no one has won yet")
		}
	}

	def hasSame(player: String, p1: Int, p2: Int, p3: Int): Boolean =
		player == board(p1) && player == board(p2) && player == board(p3) 
}

var input = List("X","O","",
			 	 "X","O","",
			 	 "", "O", "")
var board = new Board(input)
board.print()
board.check()
