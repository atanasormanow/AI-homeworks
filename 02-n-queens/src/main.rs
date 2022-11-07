use rand::distributions::{Distribution, Uniform};
use rand::seq::SliceRandom;

#[derive(Debug)]
struct BoardState {
  queens: Vec<usize>,
  queens_per_row: Vec<usize>,
  queens_per_d1: Vec<usize>,
  queens_per_d2: Vec<usize>,
}

fn diagonal_indexes(queens: &Vec<usize>, r: usize) -> (usize, usize) {
  let sum = queens[r] + r;
  let shifted_diff = queens[r] + queens.len() - r - 1;

  (shifted_diff, sum)
}

fn conflicts_by_row(board: &BoardState, r: usize) -> usize {
  let (d1i, d2i): (usize, usize) = diagonal_indexes(&board.queens, r);

  #[rustfmt::skip]
  let conflicts =
      board.queens_per_row[r]
      + board.queens_per_d1[d1i]
      + board.queens_per_d2[d2i]
      - 3;

  conflicts
}

fn init_board_permutation(n: usize) -> BoardState {
  let mut rng = rand::thread_rng();

  let mut queens: Vec<usize> = (0..n).collect();
  queens.shuffle(&mut rng);

  let queens_per_row: Vec<usize> = [1].repeat(n as usize);
  let mut queens_per_d1: Vec<usize> = [0].repeat(2 * n as usize - 1);
  let mut queens_per_d2: Vec<usize> = [0].repeat(2 * n as usize - 1);

  // D1: have the same diff (1-n)..n
  // D2: have the same sum 0..2n
  for row in 0..n {
    let (d1i, d2i) = diagonal_indexes(&queens, row);

    queens_per_d1[d1i] += 1;
    queens_per_d2[d2i] += 1;
  }

  BoardState {
    queens,
    queens_per_row,
    queens_per_d1,
    queens_per_d2,
  }
}

fn init_board_min_conflicts(n: usize) -> BoardState {
  let mut rng = rand::thread_rng();

  let mut queens: Vec<usize> = (0..n).collect();
  queens.shuffle(&mut rng);

  let queens_per_row: Vec<usize> = [1].repeat(n as usize);
  let mut queens_per_d1: Vec<usize> = [0].repeat(2 * n as usize - 1);
  let mut queens_per_d2: Vec<usize> = [0].repeat(2 * n as usize - 1);

  // D1: have the same diff (1-n)..n
  // D2: have the same sum 0..2n
  for row in 0..n {
    let (d1i, d2i) = diagonal_indexes(&queens, row);

    queens_per_d1[d1i] += 1;
    queens_per_d2[d2i] += 1;
  }

  BoardState {
    queens,
    queens_per_row,
    queens_per_d1,
    queens_per_d2,
  }
}

fn is_done(board: &BoardState) -> bool {
  #[rustfmt::skip]
  let no_conflicts =
    board
    .queens_per_row
    .iter()
    .chain(board.queens_per_d1.iter())
    .chain(board.queens_per_d2.iter())
    .all(|&queens_on_line| queens_on_line < 2);

  no_conflicts
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

fn min_conflicts_1(board: &mut BoardState, max_steps: usize) {
  for i in 0..max_steps {
    if is_done(&board) {
      print_board(&board);
    }

    //TODO
  }
}

fn main() {
  // TODO: initialize board using min conflicts (breaking ties randomly)
  // TODO: take n from user input
  // TODO: use the smallest type in terms of memmory
  let n = 5;
  let mut board: BoardState = init_board_permutation(n);

  let max_steps = 100;
  // let index = Uniform::from(0..n).sample(&mut rng);
  // println!("My random index {:?}", index);

  print_board(&board);
  println!("Initial queens: {:?}", board.queens);
  println!("Initial row conflicts: {:?}", board.queens_per_row);
  println!("Initial d1 conflicts: {:?}", board.queens_per_d1);
  println!("Initial d2 conflicts: {:?}", board.queens_per_d2);
}
// NOTE:
//   There is always only 1 queen per column.
// - pick a strategy to init the board with: random, minConf, knight jumps
