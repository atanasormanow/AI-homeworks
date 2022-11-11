use rand::distributions::{Distribution, Uniform};
use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use std::time::Instant;

#[derive(Debug)]
pub struct BoardState {
  // index = row, value = column
  queens: Vec<usize>,
  queens_per_col: Vec<i32>,
  queens_per_d1: Vec<i32>,
  queens_per_d2: Vec<i32>,
}

impl BoardState {
  pub fn is_done(&self) -> bool {
    self
      .queens_per_col
      .iter()
      .chain(self.queens_per_d1.iter())
      .chain(self.queens_per_d2.iter())
      .all(|&queens_on_line| queens_on_line < 2)
  }

  pub fn new_permutation(n: usize, rng: &mut ThreadRng) -> BoardState {
    let mut queens: Vec<usize> = (0..n).collect();
    queens.shuffle(rng);

    let queens_per_col: Vec<i32> = vec![1;n];
    let mut queens_per_d1: Vec<i32> = vec![0 ; 2 * n - 1];
    let mut queens_per_d2: Vec<i32> = vec![0 ; 2 * n - 1];

    for row in 0..n {
      let (d1i, d2i) = BoardState::diagonal_indexes(&queens, row, queens[row]);

      queens_per_d1[d1i] += 1;
      queens_per_d2[d2i] += 1;
    }

    BoardState {queens, queens_per_col, queens_per_d1, queens_per_d2}
  }

  pub fn new_min_conflicts(n: usize, rng: &mut ThreadRng) -> BoardState {
    let mut board: BoardState
      = BoardState {
        queens: vec![0; n],
        queens_per_col: vec![0; n],
        queens_per_d1: vec![0; 2 * n - 1],
        queens_per_d2: vec![0; 2 * n - 1],
      };

    let now = Instant::now();
    for row in 0..n {
      let min_conflict_move = board.min_conflict_move(row, rng);
      board.queens[row] = min_conflict_move;
      let (d1i, d2i)
        = BoardState::diagonal_indexes(&board.queens, row, min_conflict_move);

      board.queens_per_col[min_conflict_move] += 1;
      board.queens_per_d1[d1i] += 1;
      board.queens_per_d2[d2i] += 1;
    }
    println!("Min conflicts initialization in : {:?}", now.elapsed());

    board
  }

  pub fn queen_with_max_conflicts(&self, rng: &mut ThreadRng)
    -> usize {

      // Maybe use some priority queue or something
      // (queen, conflicts)
      let mut with_most_conflicts: Vec<(usize,i32)> = Vec::new();
      with_most_conflicts.push((0, i32::MIN));

      let n = self.queens.len();

      for i in 0..n {
        let (_, conf_max) = with_most_conflicts[0];
        let current_conflicts
          = BoardState::conflicts(self, i, self.queens[i]);

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

  pub fn min_conflict_move(&self, queen: usize, rng: &mut ThreadRng)
    -> usize {
      let mut with_least_conflicts: Vec<(usize,i32)> = Vec::new();
      with_least_conflicts.push((0, i32::MAX));

      let n = self.queens.len();

      for c in 0..n {
        let (_, conf_min) = with_least_conflicts[0];
        let current_conflicts = self.conflicts(queen, c);

        if current_conflicts <= conf_min {
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

  pub fn move_queen(&mut self, queen: usize, destination: usize) {
    let queen_col = self.queens[queen];
    let (queen_d1i, queen_d2i)
      = BoardState::diagonal_indexes(&self.queens, queen, queen_col);
    let (dest_d1i, dest_d2i)
      = BoardState::diagonal_indexes(&self.queens, queen, destination);

    self.queens[queen] = destination;

    self.queens_per_col[queen_col] -= 1;
    self.queens_per_col[destination] += 1;

    self.queens_per_d1[queen_d1i] -= 1;
    self.queens_per_d1[dest_d1i] += 1;

    self.queens_per_d2[queen_d2i] -= 1;
    self.queens_per_d2[dest_d2i] += 1;
  }

  pub fn print_board(&self) {
    let n = self.queens.len();
    for r in 0..n {
      for c in 0..n {
        if c == self.queens[r] {
          print!("*");
        } else {
          print!("-");
        }
      }
      println!();
    }
  }

  // Private Functions:
  //-------------------
  fn diagonal_indexes(queens: &Vec<usize>, r: usize, c: usize)
    -> (usize, usize) {

    let sum = c + r;
    let shifted_diff = c + queens.len() - r - 1;

    (shifted_diff, sum)
  }

  fn conflicts(&self, r: usize, c: usize) -> i32 {
    let (d1i, d2i) = BoardState::diagonal_indexes(&self.queens, r, c);

    let queens_count
      = self.queens_per_col[c]
      + self.queens_per_d1[d1i]
      + self.queens_per_d2[d2i];

    if c == self.queens[r] {
      return queens_count as i32 - 3;
    }
    queens_count as i32
  }

}
