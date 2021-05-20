import { readIntCodes } from "./util";
import { IntCodeMachine } from "./intCodes";
import { chunk } from "lodash";

type NIC = {
  machine: IntCodeMachine;
  queue: Point[];
};

const ptToArr = ({ x, y }: Point) => [x, y];

export default function() {
  const intCodes = readIntCodes("day23-input.txt");
  const machines: NIC[] = new Array(50)
    .fill(0)
    .map(() => ({ machine: new IntCodeMachine(intCodes), queue: [] }));

  machines.forEach((m, i) => m.machine.runToNextInput([i])); // init

  let nat: Point = { x: 0, y: 0 };
  const pokes: Point[] = [];

  let count = 0;
  while (count < 200) {
    count += 1;
    if (count > 1 && machines.every(m => m.queue.length === 0)) {
      console.log("idle on cycle", count);
      machines[0].queue.push(nat);

      if (pokes.length > 0 && pokes[pokes.length - 1].y === nat.y) {
        console.log("first y poke sent twice in a row:", nat.y);
        return;
      }

      pokes.push(nat);
    }

    for (let { machine, queue } of machines) {
      const input = queue.length > 0 ? ptToArr(queue.shift()!) : [-1];
      const output = machine.runToNextInput(input);
      for (let [dest, x, y] of chunk(output, 3)) {
        if (dest >= 0 && dest < 50) {
          machines[dest].queue.push({ x, y });
        } else if (dest === 255) {
          console.log("packet", { x, y }, "to 255 on cycle", count);
          nat = { x, y };
        }
      }
    }
  }
}
