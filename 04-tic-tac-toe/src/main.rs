use std::cmp::{max, min};
use std::io;

mod board;

use board::{
  flip_tile,
  print_board,
  print_successors,
  successors,
  terminal_state_score,
  terminal_state_winner,
  Board,
  Tile,
};

// TODO merge max_move and min_move
fn max_move((v1, b1): (i8, Board), (v2, b2): (i8, Board)) -> (i8, Board) {
  if v1 > v2 {
    (v1, b1)
  } else {
    (v2, b2)
  }
}

fn min_move((v1, b1): (i8, Board), (v2, b2): (i8, Board)) -> (i8, Board) {
  if v1 < v2 {
    (v1, b1)
  } else {
    (v2, b2)
  }
}

fn max_value(board: Board, player_symbol: Tile) -> (i8, Board) {
  match terminal_state_score(board, player_symbol) {
    Some(score) => {
      println!("Its a terminal state with score: {:?}", score);
      (score, board)
}
    None => {
      let max_of_min = |current_max: (i8, Board), b: Board| {
        max_move(current_max, min_value(b, flip_tile(player_symbol)))
      };

      let (val, succ) = successors(board, player_symbol)
        .into_iter()
        // TODO It's weird to have board as a default value
        .fold((i8::MIN, board), max_of_min);
      // TODO investigate if the player symbol is calculated correctly
      println!("Max choice with score: {val}; for player {:?}", player_symbol);
      print_board(succ);
      (val, succ)
    }
  }
}

fn min_value(board: Board, player_symbol: Tile) -> (i8, Board) {
  match terminal_state_score(board, player_symbol) {
    Some(score) => (score, board),
    None => {
      let min_of_max = |current_min: (i8, Board), b: Board| {
        min_move(current_min, max_value(b, flip_tile(player_symbol)))
      };

      let (val, succ) = successors(board, player_symbol)
        .into_iter()
        // TODO It's weird to have board as a default value
        .fold((i8::MAX, board), min_of_max);

      println!("Min choice with score: {val}; for player {:?}", player_symbol);
      print_board(succ);
      (val, succ)
    }
  }
}

fn minimax_decision(board: Board, player_symbol: Tile) -> Board {
  let (_, succ) = max_value(board, player_symbol);
  succ
}

fn main() {
  // let mut user_input = String::new();
  // let stdin = io::stdin();
  // println!("Enter tile position size (number of queens):");
  // stdin
  //   .read_line(&mut user_input)
  //   .expect("Failed to read input");

  // let nums: Vec<usize> = user_input
  //   .split_whitespace()
  //   .map(|x| x.parse::<usize>().expect("Not a natural number"))
  //   .take(2)
  //   .collect();

  // // This won't work if nums.len is 1
  // let move_pos: (usize, usize) = (nums[0], nums[1]);

  // let input = (0, 2);
  let board: Board = [
    [Tile::X, Tile::E, Tile::X],
    [Tile::X, Tile::X, Tile::O],
    [Tile::O, Tile::E, Tile::O],
  ];
  println!("Starting state:");
  print_board(board);

  println!("State score: {:?}", terminal_state_winner(board));

  // let succ = minimax_decision(board, Tile::O);
  // println!("Best move:");
  // print_board(succ);
}

// TODO:
// - take user input (x,y)
// - Use dfs
// - alternate min and max
// - terminal state score in {-1,0,1}
//   to make it optimal {D - 10, 0, 10 - D}
// - calculate "terminal" and "utility" at the same time
// ? index from 1
// * Направете играта така, че да се задава дали компютърът или играчът е първи.
// * Направете алгоритъма оптимален.
// * Choose who goes first
// - validation
