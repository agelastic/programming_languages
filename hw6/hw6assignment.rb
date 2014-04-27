
class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                   rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                   [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                    [[0, 0], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                   rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                   rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                   rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                   [[[-2,0], [0, 0], [-1, 0], [1, 0], [2, 0]], # additional 2 5-long (only needs two)
                    [[0, 0],[0,-2], [0, -1], [0, 1], [0, 2]]],
                   rotations([[0, 0], [0, 1], [1, 1]]), # additional 3
                   rotations([[0, 0], [1, 0], [0, 1], [1, 1],[-1,0]])] #additional 1

  Cheat_Piece = [[[0,0]]]

  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  def self.cheat_piece (board)
    MyPiece.new(Cheat_Piece,board)
  end

end

class MyBoard < Board

  def initialize (game)
    @cheating = 0 # is the next piece cheating?
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @current_block = MyPiece.next_piece(self)
    @score = 0
    @game = game
    @delay = 500
  end

  def set_cheat # flip the cheat bit on
    if @cheating == 0 and @score >=100
      @cheating = 1
      @score -= 100
    end
  end
  
  def store_current # need to override this because block size is variable now
    locations = @current_block.current_rotation
    size = locations.length
    displacement = @current_block.position
    (0..size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  def rotate_oneeighty
    rotate_clockwise
    rotate_clockwise
  end

  def next_piece
    if @cheating == 0 
      @current_block = MyPiece.next_piece(self)
    else
      @current_block = MyPiece.cheat_piece(self)
    end
    @current_pos = nil
    @cheating = 0
  end
  
end

class MyTetris < Tetris
  
  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_oneeighty})
    @root.bind('c', proc {@board.set_cheat})
  end
  
  def set_board 

    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end
  
end
