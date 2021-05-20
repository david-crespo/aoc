import { readFile, add, readIntCodes, binSearch } from "./util";

type Chemical = {
  n: number;
  name: string;
};

type Reaction = {
  input: Chemical[];
  output: Chemical;
};

type ReactionMap = { [k: string]: Reaction };

const T = Math.pow(10, 12);

function parseChem(s: string): Chemical {
  const [n, name] = s.split(" ");
  return { n: parseInt(n, 10), name };
}

function parseReaction(line: string): Reaction {
  const [lhs, rhs] = line.split(" => ");
  const input = lhs.split(", ").map(parseChem);
  const output = parseChem(rhs);
  return { input, output };
}

function fromEntries(entries: any[]): ReactionMap {
  const obj: { [k: string]: any } = {};
  entries.forEach(([k, v]) => {
    obj[k] = v;
  });
  return obj;
}

function zeroif(d: Count, key: string) {
  if (typeof d[key] !== "number") {
    d[key] = 0;
  }
}

function react(reaction: Reaction, onHand: Count) {
  const { input, output } = reaction;
  for (let i = 0; i < input.length; i++) {
    const c = input[i];
    zeroif(onHand, c.name);
    onHand[c.name] -= c.n;
    // if (onHand[c.name] === 0) {
    //   delete onHand[c.name];
    // }
  }

  zeroif(onHand, output.name);
  onHand[output.name] += output.n;
  // if (onHand[output.name] === 0) {
  //   delete onHand[output.name];
  // }
}

const getTarget = (onHand: Count): string | null => {
  for (let k in onHand) {
    if (k !== "ORE" && onHand[k] < 0) {
      return k;
    }
  }
  return null;
};

function makeFuel(
  reactionMap: ReactionMap,
  nFuel = 1,
  onHand: Count = { ORE: T },
): Count {
  onHand.FUEL = -nFuel;

  let target = getTarget(onHand);
  let count = 0;
  while (target && onHand.ORE > 0) {
    const reaction = reactionMap[target];
    react(reaction, onHand);
    target = getTarget(onHand);
    if (count % 1000000 === 0) {
      console.log(onHand);
      // console.log(onHand.FUEL);
    }
    count += 1;
  }

  return onHand;
}

function multOnHand(onHand: Count, mult: number) {
  Object.keys(onHand).forEach(k => {
    onHand[k] *= mult;
  });
}

const testInput = `
10 ORE => 10 A
1 ORE => 1 B
7 A, 1 B => 1 C
7 A, 1 C => 1 D
7 A, 1 D => 1 E
7 A, 1 E => 1 FUEL
`.trim();

const testInput2 = `
9 ORE => 2 A
8 ORE => 3 B
7 ORE => 5 C
3 A, 4 B => 1 AB
5 B, 7 C => 1 BC
4 C, 1 A => 1 CA
2 AB, 3 BC, 4 CA => 1 FUEL
`.trim();

const testInput3 = `
157 ORE => 5 NZVS
165 ORE => 6 DCFZ
44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
179 ORE => 7 PSHF
177 ORE => 5 HKGWZ
7 DCFZ, 7 PSHF => 2 XJWVT
165 ORE => 2 GPVTF
3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT
`.trim();

const testInput4 = `
2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
17 NVRVD, 3 JNWZP => 8 VPVL
53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
22 VJHF, 37 MNCFX => 5 FWMGM
139 ORE => 4 NVRVD
144 ORE => 7 JNWZP
5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
145 ORE => 6 MNCFX
1 NVRVD => 8 CXFTF
1 VJHF, 6 MNCFX => 4 RFSQX
176 ORE => 6 VJHF
`.trim();

export default function() {
  const input = readFile("day14-input.txt").trim();
  const reactions = testInput3.split("\n").map(parseReaction);
  const reactionMap: ReactionMap = fromEntries(
    reactions.map(r => [r.output.name, r]),
  );

  const onHand = makeFuel(reactionMap);

  const fuelReq = T - onHand.ORE;
  console.log("required for 1 FUEL", fuelReq);
  // console.log(onHand);

  // const nFuel = 100000;
  // const onHand2 = makeFuel(reactionMap, nFuel);
  // console.log(onHand2);
  // console.log(`required for ${nFuel} FUEL`, T - onHand2.ORE);

  let fuelProduced = Math.floor(T / fuelReq);
  console.log("fuel produced before secondary run", fuelProduced);
  multOnHand(onHand, fuelProduced);
  onHand.ORE = T - fuelProduced * fuelReq;
  // console.log("on hand before secondary", onHand);

  const secondaryFuelTarget = 100000;
  makeFuel(reactionMap, secondaryFuelTarget, onHand);

  console.log("on hand after secondary", onHand);
  console.log(
    "all positive?",
    Object.values(onHand).every(v => v >= 0),
  );

  console.log(
    "fuel produced after secondary run",
    fuelProduced + secondaryFuelTarget,
  );

  // while (onHand.ORE >= -T) {
  //   makeFuel(reactionMap, nFuel: 1, onHand);
  //   fuelProduced += 1;
  //   if (fuelProduced % 1000 === 0) {
  //     console.log(onHand);
  //     console.log({ fuelProduced, ore: onHand.ORE });
  //   }
  // }

  // console.log("Total produced:", fuelProduced);
  // first guess 1902655, too low
}

// baseline guess is T / 532506 = 1877913. right answer should be a few % higher
// definitive attempt is 24741 + baseline = 1902654 CRAP this is one lower than
// the other attempt which I know is too low. great
