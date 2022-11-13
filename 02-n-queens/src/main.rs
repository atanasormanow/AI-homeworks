use rand::rngs::ThreadRng;
use std::time::Instant;
use std::io;

mod board;
use board::BoardState;

fn min_conflicts(max_steps: usize, n: usize, rng: &mut ThreadRng)
  -> BoardState{

    let now = Instant::now();

    loop {
      let mut board: BoardState = BoardState::new_min_conflicts(n, rng);

      for i in 0..max_steps {
        if board.is_done() {
          println!(
            "Just found a solution in {i} steps and {:?}!",
            now.elapsed()
            );
          return board;
        }

        let max_conf_queen = board.queen_with_max_conflicts(rng);
        let min_conf_tile = board.min_conflict_move(max_conf_queen, rng);

        board.move_queen(max_conf_queen, min_conf_tile);
      }
    }
  }

fn main() {
  let mut rng: ThreadRng = rand::thread_rng();
  let max_steps = 200;

  let mut user_input = String::new();
  let stdin = io::stdin();
  println!("Enter board size (number of queens):");
  stdin.read_line(&mut user_input).ok();

  let n = user_input.trim_end().parse::<usize>();

  match n {
    Ok(n)
      => min_conflicts(max_steps, n, &mut rng).print_board(),
    Err(e) => println!("{:?}", e),
  }
}
