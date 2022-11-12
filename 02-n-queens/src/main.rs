use rand::rngs::ThreadRng;
use std::time::Instant;
use std::io;

mod board;
use board::BoardState;

fn min_conflicts(max_steps: usize, n: usize, rng: &mut ThreadRng)
  -> Option<BoardState>{

  let now = Instant::now();

  let mut board: BoardState = BoardState::new_min_conflicts(n, rng);

  for i in 0..max_steps {
    if board.is_done() {
      println!(
        "Just found a solution in {i} steps and {:?}!",
        now.elapsed()
        );
      return Some(board);
    }

    let max_conf_queen = board.queen_with_max_conflicts(rng);
    let min_conf_tile = board.min_conflict_move(max_conf_queen, rng);

    board.move_queen(max_conf_queen, min_conf_tile);
  }

  None
}

fn main() {
  let mut rng: ThreadRng = rand::thread_rng();
  let max_steps = 200;

  let mut user_input = String::new();
  let stdin = io::stdin(); // We get `Stdin` here.
  stdin.read_line(&mut user_input).ok();

  let result
    = user_input
    .trim_end()
    .parse::<usize>()
    .ok()
    .and_then(|n| min_conflicts(max_steps, n, &mut rng));

  match result {
    Some(board) => board.print_board(),
    None => println!("Solition not found :(")
  }
}

// TODO:
// - introduce random restart, to avoid loops
