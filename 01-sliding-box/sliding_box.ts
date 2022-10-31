import _ from "lodash";
import rl from "readline";

// f or pathCost is the same as path.length
type BoxState = {
  board: number[][];
  blankPosition: [number, number];
  path: string[];
};

// generte array [from..to]
function range(from: number, to: number): number[] {
  return Array.from(new Array(to - from + 1), (_, i) => i + from);
}

// Use range to generate the goal state
function generateGoalBoard(side: number): number[][] {
  let goal = range(0, side - 1).map((i) => range(side * i + 1, side * (i + 1)));
  goal[side - 1][side - 1] = 0;
  return goal;
}

// NOTE: Here arrays are serialized before comparison
//       (which seems like a slow aproach)
function isDone(node: BoxState) {
  return (
    JSON.stringify(node.board) ===
    JSON.stringify(generateGoalBoard(node.board.length))
  );
}

// A fast way to find the tile position in the goal state
function tileToGoalPosition(tile: number, side: number): [number, number] {
  if (tile === 0) {
    return [side - 1, side - 1];
  }
  return [Math.floor(tile / side), (tile + side - 1) % side];
}

// Heuristic function for Manhattan distance
function h({ board: board }: BoxState): number {
  const side = board.length;
  let totalDistance = 0;

  for (let i = 0; i < side; i++) {
    for (let j = 0; j < side; j++) {
      const tileVal = board[i][j];
      const [i0, j0] = tileToGoalPosition(tileVal, side);

      if (tileVal !== Math.pow(side, 2)) {
        totalDistance += Math.abs(i - i0) + Math.abs(j - j0);
      }
    }
  }

  return totalDistance;
}

function possibleSlides(node: BoxState): BoxState[] {
  const [bx, by] = node.blankPosition;

  // The direction of the moved tile is the dual
  // of the direction the blank "tile" is moved
  const slides = {
    down: [-1, 0],
    up: [1, 0],
    right: [0, -1],
    left: [0, 1],
  };

  const dualDir = {
    up: "down",
    down: "up",
    left: "right",
    right: "left",
  };

  return _.flatMap(_.keys(slides), (slideDir) => {
    const [dx, dy] = slides[slideDir];
    const [bdx, bdy] = [bx + dx, by + dy];

    const isValidAdj = [bdx, bdy].every((x) => _.inRange(x, node.board.length));
    const isDualOfPred = node.path[node.path.length - 1] === dualDir[slideDir];

    if (isValidAdj && !isDualOfPred) {
      let moved = _.cloneDeep(node.board);

      moved[bx][by] = moved[bdx][bdy];
      moved[bdx][bdy] = 0;

      const pathSucc = _.clone(node.path);
      pathSucc.push(slideDir);

      return [{ board: moved, blankPosition: [bdx, bdy], path: pathSucc }];
    } else {
      return [];
    }
  });
}

function IDAS(start: BoxState) {
  let result = null;
  let treshold = h(start);

  while (result === null) {
    result = limitedAStar(start, isDone, possibleSlides, treshold);
    treshold += 1;
  }

  return result;
}

function limitedAStar(
  root: BoxState,
  isDone: Function,
  expand: Function,
  treshold: number
) {
  let stack = [root];

  while (stack.length > 0) {
    // console.log("stack: ", stack);

    const current = stack[stack.length - 1];
    if (isDone(current)) {
      return stack.pop().path;
    }

    // sort in reversed order (top of stack = end of list)
    const children = expand(current).sort((a: BoxState, b: BoxState) =>
      a.path.length + h(a) >= a.path.length + h(b) ? -1 : 1
    );

    if (children.length === 0 || h(current) > treshold) {
      stack.pop();
    } else {
      children.forEach((node: BoxState) => stack.push(node));
    }
  }
  return null;
}

async function main() {
  const cli = rl.createInterface({
    input: process.stdin,
    output: process.stdout,
  });

  // Not counting the blank tile
  const size: number = await new Promise((resolve) => {
    cli.question("Enter the number of tiles: ", resolve);
  }).then((value: string) => Number.parseInt(value));

  // The position indexed from 1
  const blank: number = await new Promise((resolve) => {
    cli.question("Enter the position of the blank tile: ", resolve);
  }).then((value: string) => Number.parseInt(value));

  let lines: string[] = [];
  console.log("Enter puzzle rows:");
  for await (const line of cli) {
    lines.push(line);
    if (lines.length >= Math.sqrt(size + 1)) {
      cli.close();
    }
  }

  let initialBoard: number[][] = lines.map((line) =>
    _.split(line, " ").map((x) => Number.parseInt(x))
  );

  const side: number = initialBoard.length;

  let [bx, by]: [number, number] = [side - 1, side - 1];
  if (blank !== -1) {
    [bx, by] = tileToGoalPosition(blank, side);
  }

  const initialState: BoxState = {
    board: initialBoard,
    blankPosition: [bx, by],
    path: [],
  };

  const startTime = performance.now();
  const path = IDAS(initialState);
  const elapsed = performance.now() - startTime;

  console.log(`Elapsed time: ${elapsed} miliseconds`);
  console.log(path.length);
  path.map((x: string) => console.log(x));

  cli.close();
}
main();
