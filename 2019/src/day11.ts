import { IntCodeMachine } from "./intCodes";
import { readIntCodes, ptToStr } from "./util";

type MoveDir = "up" | "down" | "left" | "right";

function move(d: MoveDir, { x, y }: Point) {
  switch (d) {
    case "up":
      return { x, y: y - 1 };
    case "down":
      return { x, y: y + 1 };
    case "left":
      return { x: x - 1, y };
    case "right":
      return { x: x + 1, y };
  }
}

function turn(turnRight: boolean, currDir: MoveDir): MoveDir {
  if (turnRight) {
    switch (currDir) {
      case "up":
        return "right";
      case "right":
        return "down";
      case "down":
        return "left";
      case "left":
        return "up";
    }
  }

  switch (currDir) {
    case "up":
      return "left";
    case "left":
      return "down";
    case "down":
      return "right";
    case "right":
      return "up";
  }
}

function runRobot(wall: number[][], initPos: Point, initDir: MoveDir = "up") {
  const intCodes = readIntCodes("day11-input.txt");
  const machine = new IntCodeMachine(intCodes);

  let pos = initPos,
    dir: MoveDir = initDir;
  const painted = new Set();
  while (!machine.halted) {
    const [color, turnRight] = machine.runToNextInput([wall[pos.y][pos.x]]);
    wall[pos.y][pos.x] = color;
    painted.add(ptToStr(pos));
    dir = turn(!!turnRight, dir);
    pos = move(dir, pos);
  }

  // console.log("painted at least once:", painted.size);
}

function printWall(wall: number[][]) {
  wall.forEach(row => {
    console.log(row.map(z => (z ? "#" : ".")).join(""));
  });
}

export default function() {
  const wall = Array(200)
    .fill(0)
    .map(() => Array(200).fill(0));

  runRobot(wall, { x: 100, y: 100 });
  // printWall(wall);

  const wall2 = Array(120)
    .fill(0)
    .map(() => Array(120).fill(0));
  wall2[60][60] = 1;
  runRobot(wall2, { x: 60, y: 60 });
  printWall(wall2);
}
