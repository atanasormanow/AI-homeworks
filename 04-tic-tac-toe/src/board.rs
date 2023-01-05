#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Tile {
  X,
  O,
  E,
}

pub type Board = [[Tile; 3]; 3];

fn diag_wins(board: Board, player: Tile) -> bool {
  [board[0][0], board[1][1], board[2][2]]
    .into_iter()
    .all(|x| x == player)
    || [board[0][2], board[1][1], board[2][0]]
      .into_iter()
      .all(|x| x == player)
}

fn row_or_col_wins(board: Board, player: Tile) -> bool {
  for i in 0..3 {
    let row_wins = [board[i][0], board[i][1], board[i][2]]
      .into_iter()
      .all(|x| x == player);

    let col_wins = [board[0][i], board[1][i], board[2][i]]
      .into_iter()
      .all(|x| x == player);

    if row_wins || col_wins {
      return true;
    }
  }
  false
}

// Some(Tile), where Tile in {X, O} and Tile wins
// Some(E) for tie
// None when not a terminal state
// TODO remove pub
pub fn terminal_state_score(board: Board, player: Tile) -> Option<i8> {
  let op_player = flip_tile(player);
  let i_win =
    diag_wins(board, player)
    || row_or_col_wins(board, player);

  let u_win =
    diag_wins(board, player)
    || row_or_col_wins(board, player);

  if i_win {
    return Some(1);
  } else if u_win {
    return Some(-1);
  }

  let board_is_full = board
    .into_iter()
    .all(|x| x.into_iter().all(|y| y != Tile::E));

  if board_is_full {
    return Some(0);
  }

  None
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
