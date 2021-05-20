import { readIntCodes, perms } from "./util";
import { IntCodeMachine } from "./intCodes";

function runAmps(intCodes: number[], phases: number[]) {
  let nextInput = 0;
  for (let i = 0; i < 5; i++) {
    const machine = new IntCodeMachine(intCodes, phases[i]);
    const outputs = machine.runToNextInput([nextInput]);
    nextInput = outputs[0];
  }
  return nextInput;
}

function runAmpsFeedback(initIntCodes: number[], phases: number[]) {
  const machines = phases.map(phase => new IntCodeMachine(initIntCodes, phase));

  let nextInput = 0,
    running = true;

  while (running) {
    for (let i = 0; i < 5; i++) {
      const machine = machines[i];
      const outputs = machine.runToNextInput([nextInput]);
      nextInput = outputs[0];
      if (machine.halted) {
        running = false;
      }
    }
  }
  return nextInput;
}

function part1() {
  const input = readIntCodes("day7-input.txt");

  let maxThrust = 0;
  perms(new Set([0, 1, 2, 3, 4])).forEach(phases => {
    const thrust = runAmps(input, phases);
    if (thrust > maxThrust) {
      maxThrust = thrust;
    }
  });

  console.log("final max:", maxThrust);
}

export default function() {
  // part1();
  const input = readIntCodes("day7-input.txt");

  let maxThrust = 0;
  perms(new Set([5, 6, 7, 8, 9])).forEach(phases => {
    const thrust = runAmpsFeedback(input, phases);
    if (thrust > maxThrust) {
      maxThrust = thrust;
    }
  });

  console.log("final max (feedback mode):", maxThrust);
}
