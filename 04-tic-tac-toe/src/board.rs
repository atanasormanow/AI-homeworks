#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tile {
  X,
  O,
  E,
}

pub type Board = [[Tile; 3]; 3];

// Some(Tile), where Tile in {X, O} and Tile wins
// Some(E) for tie
// None when not a terminal state
// TODO remove pub
pub fn terminal_state_winner(board: Board) -> Option<Tile> {
  let mut full = true;
  let mut d1_win = true;
  let mut d2_win = true;

  for i in 0..2 {
    let i1 = i + 1;
    let mut r_win = true;
    let mut c_win = true;

    d1_win =
      d1_win
      && board[i][i] != Tile::E
      && board[i][i] == board[i1][i1];
    d2_win =
      d2_win
      && board[i][2 - i] != Tile::E
      && board[i][2 - i] == board[i1][2 - i1];

    for j in 0..2 {
      let j1 = j + 1;
      r_win = r_win && board[i][j] != Tile::E && board[i][j] == board[i][j1];
      c_win = c_win && board[j][i] != Tile::E && board[j][i] == board[j1][i];

      // NOTE: there may be duplicate checks this way,
      //       but lazy evaluation will take place
      full = full && board[i][j] != Tile::E && board[i1][j1] != Tile::E;
    }
    if r_win || c_win {
      println!("row or column {i} wins: {:?}", board[i][i]);
      return Some(board[i][i]);
    }
  }
  if d1_win || d2_win {
    println!("diagonal wins: {:?}", board[1][1]);
    return Some(board[1][1]);
  }
  if full {
    println!("Board is just full");
    return Some(Tile::E);
  }
  None
}

// TODO: better way to handle "fmap"
pub fn terminal_state_score(board: Board, player_symbol: Tile) -> Option<i8> {
  match terminal_state_winner(board) {
    Some(Tile::E) => Some(0),
    Some(c) => {
      if c == player_symbol {
        Some(1)
      } else {
        Some(-1)
      }
    }
    None => None,
  }
}

pub fn successors(board: Board, player_symbol: Tile) -> Vec<Board> {
  let mut succ_states: Vec<Board> = Vec::new();

  for i in 0..3 {
    for j in 0..3 {
      let tile: Tile = board[i][j];
      if tile == Tile::E {
        let mut succ: Board = board;
        succ[i][j] = player_symbol;
        succ_states.push(succ);
      }
    }
  }
  succ_states
}

pub fn print_board(board: Board) {
  for row in board {
    println!("{:?}", row);
  }
  println!();
}

pub fn print_successors(successors: Vec<Board>) {
  for succ in successors {
    println!("Possible next ->");
    print_board(succ);
  }
}

pub fn flip_tile(t: Tile) -> Tile {
  match t {
    Tile::X => Tile::O,
    Tile::O => Tile::X,
    _ => Tile::E,
  }
}

pub fn new_board(first_move: (usize, usize), player_symbol: Tile) -> Board {
  let mut board: Board = [[Tile::E; 3]; 3];
  board[first_move.0][first_move.1] = player_symbol;
  board
}
