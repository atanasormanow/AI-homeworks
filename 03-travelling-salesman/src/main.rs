use rand::rngs::ThreadRng;
use rand::seq::SliceRandom;
use rand::Rng;

type Gene = Vec<usize>;
type Point = (i8, i8);

struct Individual {
  gene: Gene,
  score: f32,
}

fn distance((x1, y1): Point, (x2, y2): Point) -> f32 {
  let dx: f32 = (x2 as i16 - x1 as i16).pow(2) as f32;
  let dy: f32 = (y2 as i16 - y1 as i16).pow(2) as f32;
  (dx + dy).sqrt()
}

fn score_gene(g: &Gene, points: &Vec<Point>) -> f32 {
  let n = g.len();
  let mut score: f32 = 0.0;

  for i in 1..n {
    score += distance(points[g[i]], points[g[i - 1]]);
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
  rng: &mut ThreadRng
  ) -> Vec<Individual> {
  let mut individuals: Vec<Individual> = Vec::with_capacity(n);

  for _ in 0..m {
    let gene: Gene = generate_gene(n, rng);
    let score: f32 = score_gene(&gene, &points);
    individuals.push(Individual{gene, score});
  }

  individuals
}

fn main() {
  let mut rng: ThreadRng = rand::thread_rng();
}

// NOTE:
// - N <= 100
