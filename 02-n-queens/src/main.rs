use rand::rngs::ThreadRng;

mod board;
use board::BoardState;

fn min_conflicts(max_steps: usize, n: usize, rng: &mut ThreadRng)
  -> Option<BoardState>{

  let board: BoardState = BoardState::with_permutation(n, rng);

  // Make sure this doesnt clone the memory
  let mut board: BoardState = board;

  for i in 0..max_steps {
    BoardState::print_board(&board);
    if BoardState::is_done(&board) {
      println!("Just found a solution in {i} steps!");
      return Some(board);
    }

    let max_conf_queen
      = BoardState::queen_with_max_conflicts(&board, rng);
    let min_conf_tile
      = BoardState::min_conflict_move(&board, max_conf_queen, rng);

    BoardState::move_queen(&mut board, max_conf_queen, min_conf_tile);
  }

  None
}

fn main() {
  let mut rng: ThreadRng = rand::thread_rng();
  // TODO: take n from user input
  let n = 8;

  let max_steps = 100;

  let result = min_conflicts(max_steps, n, &mut rng);

  match result {
    Some(board) => BoardState::print_board(&board),
    None => println!("Solition not found :(")
  }
}

// TODO:
// - use minConflicts to init the board
// - take n from user input
// - introduce random restart, to avoid loops
// - add timer for the computation time
// - Move struct definition of Board in a separate file and
//   move helper functions as imlementations for the struct
