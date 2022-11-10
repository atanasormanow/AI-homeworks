use rand::distributions::{Distribution, Uniform};
use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;

#[derive(Debug)]
pub struct BoardState {
  // index = row, value = column
  queens: Vec<usize>,
  queens_per_col: Vec<i32>,
  queens_per_d1: Vec<i32>,
  queens_per_d2: Vec<i32>,
}

impl BoardState {
  pub fn is_done(board: &BoardState) -> bool {
    board
      .queens_per_col
      .iter()
      .chain(board.queens_per_d1.iter())
      .chain(board.queens_per_d2.iter())
      .all(|&queens_on_line| queens_on_line < 2)
  }

  fn diagonal_indexes(queens: &Vec<usize>, r: usize, c: usize)
    -> (usize, usize) {

    let sum = c + r;
    let shifted_diff = c + queens.len() - r - 1;

    (shifted_diff, sum)
  }

  fn conflicts(board: &BoardState, r: usize, c: usize) -> i32 {
    let (d1i, d2i): (usize, usize)
                     = BoardState::diagonal_indexes(&board.queens, r, c);

    let queens_count
      = board.queens_per_col[c]
      + board.queens_per_d1[d1i]
      + board.queens_per_d2[d2i];

    if c == board.queens[r] {
      return queens_count as i32 - 3;
    }
    queens_count as i32
  }

  pub fn with_permutation(n: usize, rng: &mut ThreadRng) -> BoardState {
    let mut queens: Vec<usize> = (0..n).collect();
    queens.shuffle(rng);

    let queens_per_col: Vec<i32> = [1].repeat(n);
    let mut queens_per_d1: Vec<i32> = [0].repeat(2 * n - 1);
    let mut queens_per_d2: Vec<i32> = [0].repeat(2 * n - 1);

    for row in 0..n {
      let (d1i, d2i) = BoardState::diagonal_indexes(&queens, row, queens[row]);

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

  pub fn queen_with_max_conflicts(board: &BoardState, rng: &mut ThreadRng)
    -> usize {

      // Maybe use some priority queue or something
      // (queen, conflicts)
      let mut with_most_conflicts: Vec<(usize,i32)> = Vec::new();
      with_most_conflicts.push((0, i32::MIN));

      let n = board.queens.len();

      for i in 0..n {
        let (_, conf_max) = with_most_conflicts[0];
        let current_conflicts
          = BoardState::conflicts(board, i, board.queens[i]);

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

  pub fn min_conflict_move(
    board: &BoardState,
    queen: usize,
    rng: &mut ThreadRng
    ) -> usize {
      // Maybe use some priority queue or something
      // (queen, conflicts)
      let mut with_least_conflicts: Vec<(usize,i32)> = Vec::new();
      with_least_conflicts.push((0, i32::MAX));

      let n = board.queens.len();

      for c in 0..n {
        let (_, conf_min) = with_least_conflicts[0];
        let current_conflicts = BoardState::conflicts(board, queen, c);

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

  pub fn move_queen(board: &mut BoardState, queen: usize, destination: usize) {
    println!("moving quuen No: {:?}", queen + 1);

    let queen_col = board.queens[queen];
    let (queen_d1i, queen_d2i)
      = BoardState::diagonal_indexes(&board.queens, queen, queen_col);
    let (dest_d1i, dest_d2i)
      = BoardState::diagonal_indexes(&board.queens, queen, destination);

    board.queens[queen] = destination;

    board.queens_per_col[queen_col] -= 1;
    board.queens_per_col[destination] += 1;

    board.queens_per_d1[queen_d1i] -= 1;
    board.queens_per_d1[dest_d1i] += 1;

    board.queens_per_d2[queen_d2i] -= 1;
    board.queens_per_d2[dest_d2i] += 1;
  }

  pub fn print_board(board: &BoardState) {
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
}
