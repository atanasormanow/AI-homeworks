use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use rand::Rng;
use std::env;

type Gene = Vec<usize>;
type Point = (i16, i16);

#[derive(Debug)]
struct Individual {
  gene: Gene,
  score: f32,
}

fn distance((x1, y1): Point, (x2, y2): Point) -> f32 {
  let dx = x2.wrapping_sub(x1) as f32;
  let dy = y2.wrapping_sub(y1) as f32;
  (dx * dx + dy * dy).sqrt()
}

fn score_gene(g: &Gene, points: &Vec<Point>) -> f32 {
  let n = g.len();
  let mut score: f32 = 0.0;

  for i in 1..n {
    let d = distance(points[g[i]], points[g[i - 1]]);
    score += d;
  }

  score
}

// A path that goes trough all points is a permutation of those points
fn generate_gene(n: usize, rng: &mut ThreadRng) -> Gene {
  let mut indexed_points: Vec<usize> = (0..n).collect();
  indexed_points.shuffle(rng);
  indexed_points
}

fn generate_points(n: usize, rng: &mut ThreadRng) -> Vec<Point> {
  let mut points: Vec<Point> = Vec::with_capacity(n);

  for _ in 0..n {
    let p: Point = rng.gen();
    points.push(p);
  }

  points
}

fn generate_individuals(
  n: usize,
  m: usize,
  points: &Vec<Point>,
  rng: &mut ThreadRng,
) -> Vec<Individual> {
  let mut individuals: Vec<Individual> = Vec::with_capacity(n);

  for _ in 0..m {
    let gene: Gene = generate_gene(n, rng);
    let score: f32 = score_gene(&gene, &points);
    individuals.push(Individual { gene, score });
  }

  individuals
}

fn main() {
  env::set_var("RUST_BACKTRACE", "1");

  let n = 10;
  let gen_size = 5;
  let mut rng: ThreadRng = rand::thread_rng();
  let points = generate_points(n, &mut rng);
  let individuals = generate_individuals(n, gen_size, &points, &mut rng);

  println!("{:?}", individuals);
}

// NOTE:
// - N <= 100
