import { reverse } from "lodash";

const paramCounts = new Map([
  [1, 2],
  [2, 2],
  [3, 1],
  [4, 1],
  [5, 2],
  [6, 2],
  [7, 3],
  [8, 3],
  [99, 0],
]);

function padZeroes(arr: number[], targetLength: number) {
  if (arr.length < targetLength) {
    const zeroes: number[] = Array(targetLength - arr.length).fill(0);
    return [...arr, ...zeroes];
  }
  return arr;
}

function parseOpcode(val: number) {
  const opcode = parseInt(val.toString().slice(-2), 10);
  let pModes = Array.from(val.toString().slice(0, -2))
    .reverse()
    .map(s => parseInt(s, 10));

  const paramCount = paramCounts.get(opcode) || 0;
  pModes = padZeroes(pModes, paramCount);

  return { opcode, pModes };
}

export class IntCodeMachine {
  intCodes: number[];
  cursor: number;
  relBase: number;
  halted: boolean;

  constructor(intCodes: number[], phase?: number) {
    this.intCodes = [...intCodes];
    this.cursor = 0;
    this.relBase = 0;
    this.halted = false;

    if (phase != null) {
      this.runToNextInput([phase]);
    }
  }

  getParams(n: number, pModes: number[], leaveLastRaw = false) {
    const raw = this.intCodes.slice(this.cursor + 1, this.cursor + 1 + n);
    // console.log("raw: ", raw);
    return raw.map((p, idx) => {
      const mode = pModes[idx];

      if (leaveLastRaw && idx === n - 1) {
        return mode === 2 ? this.relBase + p : p;
      }

      switch (mode) {
        case 0: // positional
        default:
          return this.intCodes[p] || 0;
        case 1: // immediate
          return p;
        case 2: // relative
          return this.intCodes[this.relBase + p] || 0;
      }
    });
  }

  runToNextInput(inputQueue: number[] = []) {
    const outputs = [];
    while (!this.halted) {
      const { opcode, pModes } = parseOpcode(this.intCodes[this.cursor]);
      // console.log(opcode, pModes, this.relBase, outputs);
      if (opcode === 1) {
        const [val1, val2, targetIdx] = this.getParams(3, pModes, true);
        // console.log("params: ", [val1, val2, targetIdx]);
        this.intCodes[targetIdx] = val1 + val2;
        this.cursor += 4;
      } else if (opcode === 2) {
        const [val1, val2, targetIdx] = this.getParams(3, pModes, true);
        this.intCodes[targetIdx] = val1 * val2;
        this.cursor += 4;
      } else if (opcode === 3) {
        const p1 = this.intCodes[this.cursor + 1];
        const input = inputQueue.shift();
        if (input == null) {
          return outputs;
        }
        if (pModes.length > 0 && pModes[0] === 2) {
          this.intCodes[p1 + this.relBase] = input;
        } else {
          this.intCodes[p1] = input;
        }
        this.cursor += 2;
      } else if (opcode === 4) {
        const [val] = this.getParams(1, pModes);
        outputs.push(val);
        this.cursor += 2;
      } else if (opcode === 5) {
        const [val1, val2] = this.getParams(2, pModes);
        if (val1) {
          this.cursor = val2;
        } else {
          this.cursor += 3;
        }
      } else if (opcode === 6) {
        const [val1, val2] = this.getParams(2, pModes);
        if (!val1) {
          this.cursor = val2;
        } else {
          this.cursor += 3;
        }
      } else if (opcode === 7) {
        const [val1, val2, targetIdx] = this.getParams(3, pModes, true);
        this.intCodes[targetIdx] = val1 < val2 ? 1 : 0;
        this.cursor += 4;
      } else if (opcode === 8) {
        const [val1, val2, targetIdx] = this.getParams(3, pModes, true);
        // console.log("params: ", [val1, val2, targetIdx]);
        this.intCodes[targetIdx] = val1 == val2 ? 1 : 0;
        this.cursor += 4;
      } else if (opcode === 9) {
        const [val1] = this.getParams(1, pModes);
        this.relBase += val1;
        this.cursor += 2;
      } else if (opcode === 99) {
        this.halted = true;
        return outputs;
      }
    }
    return outputs;
  }

  runAscii(input: string = "") {
    const inputQueue = Array.from(input).map(s => s.charCodeAt(0));
    const output = this.runToNextInput(inputQueue);
    return output.every(c => c <= 127)
      ? String.fromCharCode(...output)
      : output;
  }
}
