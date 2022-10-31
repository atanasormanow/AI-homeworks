import _ from "lodash";
import rl from "readline";

// f or pathCost is the same as path.length
type BoxState = {
  board: number[][];
  blankPosition: [number, number];
  path: string[];
};

// A fast way to find the tile position in the goal state
function tileToGoalPosition(tile: number, side: number): [number, number] {
  if (tile === 0) return [side - 1, side - 1];
  else return [Math.ceil(tile / side) - 1, (tile + side - 1) % side];
}

// Heuristic function for Manhattan distance
function h({ board: board }: BoxState): number {
  const side = board.length;
  let totalDistance = 0;

  for (let i = 0; i < side; i++) {
    for (let j = 0; j < side; j++) {
      const tileVal = board[i][j];
      const [i0, j0] = tileToGoalPosition(tileVal, side);

      if (tileVal !== 0) {
        totalDistance += Math.abs(i - i0) + Math.abs(j - j0);
      }
    }
  }

  return totalDistance;
}

function possibleSlides(
  node: BoxState,
  visitedBoards: number[][][]
): BoxState[] {
  const [bx, by] = node.blankPosition;

  // The direction of the moved tile is the dual
  // of the direction the blank "tile" is moved
  const slides = {
    down: [-1, 0],
    up: [1, 0],
    right: [0, -1],
    left: [0, 1],
  };

  return _.flatMap(_.keys(slides), (slideDir) => {
    const [dx, dy] = slides[slideDir];
    const [bdx, bdy] = [bx + dx, by + dy];

    const isValidAdj = [bdx, bdy].every((x) => _.inRange(x, node.board.length));

    if (isValidAdj) {
      let moved = _.cloneDeep(node.board);

      moved[bx][by] = moved[bdx][bdy];
      moved[bdx][bdy] = 0;

      const wasVisited = _.some(visitedBoards, (vb) => _.isEqual(vb, moved));
      if (wasVisited) return [];

      const pathSucc = _.clone(node.path);
      pathSucc.push(slideDir);

      return [{ board: moved, blankPosition: [bdx, bdy], path: pathSucc }];
    } else return [];
  });
}

function IDAS(start: BoxState) {
  let result = null;
  let treshold = h(start);

  while (result === null) {
    console.log("HERE WE GO AGAIN!");
    result = limitedAStar(start, possibleSlides, treshold);
    // moves most often change the h value with 2
    treshold += 1;
  }

  return result;
}

function limitedAStar(root: BoxState, expand: Function, treshold: number) {
  let stack = [root];
  let visitedBoards = [];

  while (stack.length > 0) {
    // console.log("stack: ", stack);

    // if i keep current in the stack, ill have the path
    // in the stack at the end, but that will cost some memory
    const current = stack.pop();
    if (h(current) === 0) {
      return current.path;
    }
    visitedBoards.push(current.board);

    console.log(h(current) + current.path.length, current.board);

    if (h(current) + current.path.length <= treshold) {
      // console.log(`Значи твърдиш че ${h(current)} + ${current.path.length} <= ${treshold}`);
      // sort in reversed order (top of stack = end of list)
      const children = expand(current, visitedBoards).sort(
        (a: BoxState, b: BoxState) =>
          a.path.length + h(a) >= a.path.length + h(b) ? -1 : 1
      );
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
