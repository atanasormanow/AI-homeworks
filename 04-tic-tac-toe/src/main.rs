use std::cmp::{max, min};
use std::io;

mod board;

use board::{
  flip_tile, print_board, print_successors, successors, terminal_state_score, Board, Tile,
};

fn max_value(board: Board, player_symbol: Tile) -> i8 {
  match terminal_state_score(board, player_symbol) {
    Some(score) => {
      println!(
        "Its a terminal state with score {:?} for player {:?}",
        score, player_symbol
      );
      print_board(board);
      score
    }
    None => successors(board, player_symbol)
      .into_iter()
      .fold(i8::MIN, |current_max, b| {
        let min_val = min_value(b, flip_tile(player_symbol));
        println!("Choosing the best of {:?} and {:?} ", current_max, min_val);
        println!("And currently looking at (for player {:?}):", player_symbol);
        print_board(b);
        max(current_max, min_val)
      }),
  }
}

fn min_value(board: Board, player_symbol: Tile, i) -> i8 {
  -1

  // match terminal_state_score(board, player_symbol) {
  //   Some(score) => {
  //     println!(
  //       "Its a terminal state with score {:?} for player {:?}",
  //       score, player_symbol
  //     );
  //     print_board(board);
  //     score
  //   }
  //   None => successors(board, player_symbol)
  //     .into_iter()
  //     .fold(i8::MAX, |current_min, b| {
  //       let max_val = max_value(b, flip_tile(player_symbol));
  //       println!("Choosing the worst of {:?} and {:?} ", current_min, max_val);
  //       println!("And currently looking at (for player {:?}):", player_symbol);
  //       print_board(b);
  //       min(current_min, max_val)
  //     }),
  // }
}

fn minimax_decision(board: Board, player_symbol: Tile) -> Board {
  match terminal_state_score(board, player_symbol) {
    Some(score) => {
      println!(
        "Game over, player with score {:?} for player {:?} wins",
        score, player_symbol
      );
      print_board(board);
      board
    }
    None => {
      let (_, next_best) = successors(board, player_symbol).into_iter().fold(
        (i8::MIN, board),
        |(curr_min, b), succ| {
          let min_val = min_value(succ, player_symbol);
          if min_val > curr_min {
            return (min_val, succ);
          } else {
            return (curr_min, b);
          }
        },
      );
      next_best
    }
  }
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
    [Tile::O, Tile::X, Tile::E],
    [Tile::O, Tile::X, Tile::E],
    [Tile::X, Tile::O, Tile::E],
  ];

  let first_player = Tile::X;
  println!("Starting state (player {:?}'s turn):", first_player);
  print_board(board);

  let succ = minimax_decision(board, Tile::O);
  println!("Best move:");
  print_board(succ);
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
