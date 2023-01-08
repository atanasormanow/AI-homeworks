import _, { attempt } from "lodash";
import { readFile, writeFile } from "fs/promises";

type Point = {
  x: number,
  y: number
}

type Centroid = Point;

type Cluster = {
  centroid: Centroid,
  points: Point[]
}

function parseDataPoints(content: string, delimiter: string): Point[] {
  return content
    .split('\n')
    .map(entry => {
      const sep = entry.split(delimiter)
      return { x: +sep[0], y: +sep[1] }
    });
}

function distance({ x, y }: Point, { x: x0, y: y0 }: Point): number {
  return Math.sqrt(Math.pow(x - x0, 2) + Math.pow(y - y0, 2));
}

function meanCoords(points: Point[]): Point {
  const [xsum, ysum] =
    _.reduce(points, ([xs, ys], { x, y }) => [xs + x, ys + y], [0, 0]);
  return { x: xsum / points.length, y: ysum / points.length };
}

// Find the new centroid location for a cluster
function updateCentroid({ points: p }: Cluster): Centroid {
  return meanCoords(p);
}

// find the closest centroid to a given point
function findClosestCentroid(centroids: Centroid[], p: Point): String {
  const [_d, closestCentroid]: [number, Point] =
    _.reduce(centroids,
      ([d, c0], c) => {
        const currentDistance = distance(p, c);

        if (currentDistance < d) {
          return [currentDistance, c];
        } else return [d, c0];
      },
      [Infinity, p]);

  // return String in order for groupBy to work
  return JSON.stringify(closestCentroid);
}

function formClusters(centroids: Centroid[], points: Point[]): Cluster[] {
  const newClusters =
    _.groupBy(points, p => findClosestCentroid(centroids, p));

  return _.keys(newClusters)
    .map(k => {
      return { centroid: JSON.parse(k), points: newClusters[k] }
    });
}

// The sum of distances of all points to their centroid (the lower the better)
function withinPointScatter(clusters: Cluster[]): number {
  const scatterPerCluster = ({ centroid, points }: Cluster) => {
    return _.reduce(points, (acc, p) => acc + distance(p, centroid), 0);
  }

  return _.reduce(clusters, (acc, c) => acc + scatterPerCluster(c), 0);
}

function getRandomInt(max: number): number {
  return Math.floor(Math.random() * max);
}

function kRandomCentroids(k: number, points: Point[]): Centroid[] {
  return _.range(k).map(_x => points[getRandomInt(points.length)]);
}

// const farthestPoint = (c: Centroid, ps: Point[]): Point => {
//   const [distance, point] =
//     _.reduce(
//       ps,
//       ([d, p0], p) => {
//         const currentDistance = distance(p, c);

//         if (currentDistance > d) {
//           return [d, p0]
//         } else return [currentDistance, p];
//       },
//       [-Infinity, c]
//     );
//   return point;
// }
function kFarApartCentroids(k: number, points: Point[]): Centroid[] {
  const generateRandomCentroids = () => {
    return _.range(k).map(_x => points[getRandomInt(points.length)]);
  }

  const scatter = (ps: Point[], m: Point) => {
    return _.reduce(ps, (acc, p) => acc + distance(p, m), 0);
  }

  const attempts = 10000;
  let bestCentroids = generateRandomCentroids();
  let bestDistance = scatter(bestCentroids, meanCoords(bestCentroids));

  for (let i = 0; i < attempts; i++) {
    const newCentroids = generateRandomCentroids();
    const newDistance = scatter(newCentroids, meanCoords(newCentroids));

    if (newDistance > bestDistance) {
      bestDistance = newDistance;
      bestCentroids = newCentroids;
    }
  }

  return bestCentroids;
}

// Update cluster locations to the avg of its points and form new clusters
// Do this until a fixed point is reached
function stabilizeCentroids(clusters: Cluster[], points: Point[]): Cluster[] {
  let newClusters: Cluster[] = clusters;
  let newCentroids: Centroid[] = clusters.map(updateCentroid);

  while (
    !_.isEqual(newClusters.map(({ centroid }) => centroid), newCentroids)
  ) {
    newClusters = formClusters(newCentroids, points);
    newCentroids = newClusters.map(updateCentroid);
  }

  return newClusters;
}

function saveClusters(clusters: Cluster[], file: string) {
  // TODO maybe build this in the Centroid type itself
  let localColors =
    ['purple', 'blue', 'pink', 'yellow', 'green', 'cyan', 'orange', 'red'];

  const flattenCluster =
    ({ points }: Cluster, color: string): string[] => {
      return points.map(({ x, y }) => [x, y, color + '\n'].toString())
    }

  const stringifiedClusters =
    _.flatMap(clusters, cluster => flattenCluster(cluster, localColors.pop()));

  writeFile(file, stringifiedClusters).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

function testKValues(points: Point[]) {
  const tests: string[] = _.range(2, 8).map(i => {
    const centroids = kRandomCentroids(i, points);
    const clusters = formClusters(centroids, points);
    const stableClusters = stabilizeCentroids(clusters, points);
    return [i, withinPointScatter(stableClusters)].toString() + '\n';
  });

  writeFile('out/kTests.txt', tests).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

function kMeans(k: number, file: string): Promise<Cluster[]> {
  return readFile(file).then(content => {
    const points = parseDataPoints(content.toString(), ' ');
    const centroids = kRandomCentroids(k, points);
    const clusters = formClusters(centroids, points);
    return stabilizeCentroids(clusters, points);
    // saveClusters(stableClusters, 'out/output.txt');
  }).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

function chooseBestAmongK(k: number, file: string) {
  readFile(file).then(content => {
    const points = parseDataPoints(content.toString(), ' ');
    let bestScore: number = Infinity;
    let bestClusters: Cluster[] = [];

    for (let i = 0; i < k; i++) {
      const centroids = kFarApartCentroids(8, points);
      const clusters = formClusters(centroids, points);
      const finalClusters = stabilizeCentroids(clusters, points);
      const score = withinPointScatter(finalClusters);
      console.log("Clustering with internal scatter: ", score);

      if (score < bestScore) {
        bestScore = score;
        bestClusters = finalClusters;
      }
    }
    console.log("Clustering with the BEST internal scatter: ", bestScore);
    saveClusters(bestClusters, 'out/output.txt');
  }).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

chooseBestAmongK(100, 'static/unbalance.txt');

// kMeans(4, "static/normal.txt");
// kMeans(8, "static/unbalance.txt");

// use the elbow method to pick k
// testKValues(points);

// TODO's:
// - kMeans++
// - save cluster locations in separate file and plot them
