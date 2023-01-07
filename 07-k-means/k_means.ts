import _ from "lodash";
import { readFile } from "fs/promises";

type Point = {
  x: number,
  y: number
}

type Centroid = Point;

type Cluster = {
  centroid: Centroid,
  points: Point[]
}

function parseDataPoints(content: string): Point[] {
  return content
    .split('\n')
    .map(entry => {
      const sep = entry.split('\t')
      return { x: +sep[0], y: +sep[1] }
    });
}

// Find the new centroid location for a cluster
function updateCentroid({ points: p }: Cluster): Centroid {
  const [xsum, ysum] =
    _.reduce(p, ([xs, ys], { x, y }) => [xs + x, ys + y], [0, 0]);
  return { x: xsum / p.length, y: ysum / p.length };
}

// find the closest centroid to a given point
function findClosestCentroid(centroids: Centroid[], { x, y }: Point): String {
  const [_d, closestCentroid]: [number, Point] =
    _.reduce(centroids,
      ([d, c0], { x: x1, y: y1 }) => {

        const currentDistance
          = Math.sqrt(Math.pow(x - x1, 2) + Math.pow(y - y1, 2))

        if (currentDistance < d) {
          return [currentDistance, { x: x1, y: y1 }];
        } else return [d, c0];
      },
      [Infinity, { x, y }]);

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

function getRandomFloat(min: number, max: number, decimals: number) {
  const str = (Math.random() * (max - min) + min).toFixed(decimals);
  return parseFloat(str);
}

function kRandomCentroids(k: number): Centroid[] {
  return _.range(k).map(_x => {
    return {
      x: getRandomFloat(1, 10, 3),
      y: getRandomFloat(1, 10, 3)
    }
  });
}

// Update cluster locations to the avg of its points and form new clusters
function step(clusters: Cluster[], points: Point[]): Cluster[] {
  const newCentroids: Centroid[] = clusters.map(updateCentroid);
  return formClusters(newCentroids, points);
}

export async function getPoints() {
  readFile("static/normal.txt").then(content => {
    return parseDataPoints(content.toString());
  }).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

async function main() {
  readFile("static/normal.txt").then(content => {
    const points = parseDataPoints(content.toString());
    const centroids = kRandomCentroids(5);
    const clusters = formClusters(centroids, points);
    console.log(clusters);

  }).catch(error => {
    console.error(error.message);
    process.exit(1);
  });
}

main();

// TODO's:
// - determine number of clusters
// - score clusterization
// - determine step iterations based on the score
// - add random restart and compare scores
// - fix exporting to render.js (used in the html)
// - add colors once rendering works
