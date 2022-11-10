use rand::distributions::{Distribution, Uniform};
use rand::seq::SliceRandom;
use rand::rngs::ThreadRng;

#[derive(Debug)]
struct BoardState {
  // index = row, value = column
  queens: Vec<usize>,
  queens_per_col: Vec<i32>,
  queens_per_d1: Vec<i32>,
  queens_per_d2: Vec<i32>,
}

fn diagonal_indexes(queens: &Vec<usize>, r: usize, c: usize) -> (usize, usize) {
  let sum = c + r;
  let shifted_diff = c + queens.len() - r - 1;

  (shifted_diff, sum)
}

fn conflicts(board: &BoardState, r: usize, c: usize) -> i32 {
  let (d1i, d2i): (usize, usize)
                   = diagonal_indexes(&board.queens, r, c);

  let queens_count
    = board.queens_per_col[c]
    + board.queens_per_d1[d1i]
    + board.queens_per_d2[d2i];

  if c == board.queens[r] {
    return queens_count as i32 - 3;
  }
  queens_count as i32
}

fn init_board_permutation(n: usize, rng: &mut ThreadRng) -> BoardState {
  let mut queens: Vec<usize> = (0..n).collect();
  queens.shuffle(rng);

  let queens_per_col: Vec<i32> = [1].repeat(n);
  let mut queens_per_d1: Vec<i32> = [0].repeat(2 * n - 1);
  let mut queens_per_d2: Vec<i32> = [0].repeat(2 * n - 1);

  for row in 0..n {
    let (d1i, d2i) = diagonal_indexes(&queens, row, queens[row]);

    queens_per_d1[d1i] += 1;
    queens_per_d2[d2i] += 1;
  }

  BoardState {
    queens,
    queens_per_col,
    queens_per_d1,
    queens_per_d2,
  }
}

// TODO
fn init_board_min_conflicts(n: usize) -> BoardState {
  let mut rng = rand::thread_rng();

  let mut queens: Vec<usize> = (0..n).collect();
  queens.shuffle(&mut rng);

  let queens_per_col: Vec<i32> = [1].repeat(n);
  let mut queens_per_d1: Vec<i32> = [0].repeat(2 * n - 1);
  let mut queens_per_d2: Vec<i32> = [0].repeat(2 * n - 1);

  for row in 0..n {
    let (d1i, d2i) = diagonal_indexes(&queens, row, queens[row]);

    queens_per_d1[d1i] += 1;
    queens_per_d2[d2i] += 1;
  }

  BoardState {
    queens,
    queens_per_col,
    queens_per_d1,
    queens_per_d2,
  }
}

fn is_done(board: &BoardState) -> bool {
  board
    .queens_per_col
    .iter()
    .chain(board.queens_per_d1.iter())
    .chain(board.queens_per_d2.iter())
    .all(|&queens_on_line| queens_on_line < 2)
}

fn print_board(board: &BoardState) {
  let n = board.queens.len();
  for r in 0..n {
    for c in 0..n {
      if c == board.queens[r] {
        print!("*");
      } else {
        print!("-");
      }
    }
    println!();
  }
}

// 1) find the queen with most conflicts
// 2) find the move dest with least conflicts
//  - break ties randomly
// 3) return the new state after the move
fn queen_with_max_conflicts(board: &BoardState, rng: &mut ThreadRng)
  -> usize {

  // Maybe use some priority queue or something
  // (queen, conflicts)
  let mut with_most_conflicts: Vec<(usize,i32)> = Vec::new();
  with_most_conflicts.push((0, i32::MIN));

  let n = board.queens.len();

  for i in 0..n {
    let (_, conf_max) = with_most_conflicts[0];
    let current_conflicts = conflicts(board, i, board.queens[i]);

    if current_conflicts >= conf_max{
      if current_conflicts != conf_max {
        with_most_conflicts.clear();
      }
      with_most_conflicts.push((i, current_conflicts));
    }
  }

  let to_move = Uniform::from(0..with_most_conflicts.len()).sample(rng);
  let (queen, _) = with_most_conflicts[to_move];

  queen
}

fn min_conflict_move(board: &BoardState, queen: usize, rng: &mut ThreadRng)
  -> usize {
  // Maybe use some priority queue or something
  // (queen, conflicts)
  let mut with_least_conflicts: Vec<(usize,i32)> = Vec::new();
  with_least_conflicts.push((0, i32::MAX));

  let n = board.queens.len();

  for c in 0..n {
    let (_, conf_min) = with_least_conflicts[0];
    let current_conflicts = conflicts(board, queen, c);

    if current_conflicts <= conf_min{
      if current_conflicts != conf_min {
        with_least_conflicts.clear();
      }
      with_least_conflicts.push((c, current_conflicts));
    }
  }

  let to_move = Uniform::from(0..with_least_conflicts.len()).sample(rng);
  let (destination, _) = with_least_conflicts[to_move];

  destination
}

fn move_queen(board: &mut BoardState, queen: usize, destination: usize) {
    println!("moving quuen No: {:?}", queen + 1);

    let queen_col = board.queens[queen];
    let (queen_d1i, queen_d2i)
      = diagonal_indexes(&board.queens, queen, queen_col);
    let (dest_d1i, dest_d2i)
      = diagonal_indexes(&board.queens, queen, destination);

    board.queens[queen] = destination;

    board.queens_per_col[queen_col] -= 1;
    board.queens_per_col[destination] += 1;

    board.queens_per_d1[queen_d1i] -= 1;
    board.queens_per_d1[dest_d1i] += 1;

    board.queens_per_d2[queen_d2i] -= 1;
    board.queens_per_d2[dest_d2i] += 1;
}

fn min_conflicts(board: BoardState, max_steps: usize, rng: &mut ThreadRng)
  -> Option<BoardState>{

  // Make sure this doesnt clone the memory
  let mut board: BoardState = board;

  for i in 0..max_steps {
    print_board(&board);
    if is_done(&board) {
      println!("Just found a solution in {i} steps!");
      return Some(board);
    }

    let max_conf_queen = queen_with_max_conflicts(&board, rng);
    let min_conf_tile = min_conflict_move(&board, max_conf_queen, rng);

    move_queen(&mut board, max_conf_queen, min_conf_tile);
  }

  None
}

fn main() {
  let mut rng: ThreadRng = rand::thread_rng();
  // TODO: take n from user input
  let n = 8;
  let board: BoardState = init_board_permutation(n, &mut rng);

  let max_steps = 100;

  let result = min_conflicts(board, max_steps, &mut rng);

  match result {
    Some(board) => print_board(&board),
    None => println!("Solition not found :(")
  }
}

// TODO:
// - use minConflicts to init the board
// - take n from user input
// - introduce random restart, to avoid loops
// - Move struct definition of Board in a separate file and
//   move helper functions as imlementations for the struct
