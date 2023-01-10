import { map, range, shuffle, chain, reduce } from "lodash";

type Point = {
  x: number,
  y: number
};

type Fitness = number;

type Individual = number[];

type Population = Individual[];

type FitPopulation = {
  individual: Individual,
  fitness: number
}[];

type Config = {
  bestToKeep: number,
  mutationChance: number,
};

const citiesName = [
  "Aberystwyth", "Brighton", "Edinburgh", "Exeter", "Glasgow", "Inverness",
  "Liverpool", "London", "Newcastle", "Nottingham", "Oxford", "Stratford"
];

const citiesXY = [
  [0.190032e-03, -0.285946e-03],
  [383.458, -0.608756e-03],
  [-27.0206, -282.758],
  [335.751, -269.577],
  [69.4331, -246.780],
  [168.521, 31.4012],
  [320.350, -160.900],
  [179.933, -318.031],
  [492.671, -131.563],
  [112.198, -110.561],
  [306.320, -108.090],
  [217.343, -447.089]
];

function createPopulation(numberOfIndividuals: number, geneSize: number)
  : Population {
  return range(numberOfIndividuals).map(_ => shuffle(range(geneSize)));
}

function distance({ x, y }: Point, { x: x0, y: y0 }: Point): number {
  return Math.sqrt(Math.pow(x - x0, 2) + Math.pow(y - y0, 2));
}

function score([h, ...t]: Individual, points: Point[]): Fitness {
  const op = ([d, prev], curr: number) => {
    return [d + distance(points[prev], points[curr]), curr];
  }
  const [totalDistance, _] = reduce(t, op, [0, h]);
  return totalDistance;
}

function evalFitness(population: Population, points: Point[]): FitPopulation {
  return map(population, ind => {
    return {
      individual: ind,
      fitness: score(ind, points)
    }
  });
}

function select(fitPopulation: FitPopulation, { bestToKeep }: Config) {
}

function evolve(population, points, config) {
  chain(population)
    .evalFitness(points)
    .select(config)
    .cross()
    .mutate(config)
}

function main() {
  const populationSize = 100;
  const bestToKeep = 10;
  const geneSize = citiesXY.length;
  const points = citiesXY;
  const config = { bestToKeep: 10, mutationChance: 0.05 }

  const population = createPopulation(populationSize, geneSize);
  const firstCycle = evolve(population, points, config);

}

// evolve points proportions mutations =
//   mutate mutations
//     . crossSelected
//     . select proportions
//     . sortPopulation
//     . evalFitness points

main();

// TODOs:
