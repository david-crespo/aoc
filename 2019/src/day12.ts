import { add, lcm } from "./util";

type Body = {
  x: number;
  y: number;
  z: number;
  vx: number;
  vy: number;
  vz: number;
};

type Axis = {
  p: number;
  v: number;
};

const compare = (a: number, b: number) => (b > a ? 1 : b < a ? -1 : 0);

function tick(moons: Body[], pairs: Body[][]) {
  // gravity
  for (let i = 0; i < pairs.length; i++) {
    const [m1, m2] = pairs[i];

    m1.vx += compare(m1.x, m2.x);
    m1.vy += compare(m1.y, m2.y);
    m1.vz += compare(m1.z, m2.z);
  }

  // velocity
  for (let i = 0; i < moons.length; i++) {
    const m = moons[i];
    m.x += m.vx;
    m.y += m.vy;
    m.z += m.vz;
  }
}

const energy = (moons: Body[]) =>
  moons
    .map(
      m =>
        [m.x, m.y, m.z].map(Math.abs).reduce(add) *
        [m.vx, m.vy, m.vz].map(Math.abs).reduce(add),
    )
    .reduce(add);

function energyAfterTicks(moons: Body[], n: number) {
  const pairs = getPairs(moons);
  for (let i = 0; i < n; i++) {
    tick(moons, pairs);
  }
  console.log(energy(moons));
}

const match = (m1?: Body, m2?: Body) => {
  if (!m1 || !m2) return false;
  return (
    m1.x === m2.x &&
    m1.y === m2.y &&
    m1.z === m2.z &&
    m1.vx === m2.vx &&
    m1.vy === m2.vy &&
    m1.vz === m2.vz
  );
};

const M = 1000000;
const B = 1000000000;
const INT = 10 * M;

function ticksToRepeat(moons: Body[]): number {
  const pairs = getPairs(moons);
  const orig = [...moons.map(m => ({ ...m }))];

  let count = 0;
  // const cap = 50 * M;
  // while (true && count < cap) {
  while (true) {
    tick(moons, pairs);
    count += 1;
    if (count % INT === 0) console.log(count / 1000000, "M");

    if (
      match(moons[0], orig[0]) &&
      match(moons[1], orig[1]) &&
      match(moons[2], orig[2]) &&
      match(moons[3], orig[3])
    ) {
      return count;
    }
  }
  return count;
}

function getPairs(moons: Body[]): Body[][] {
  const pairs: Body[][] = [];
  moons.forEach(m1 => {
    moons.forEach(m2 => {
      if (m1 !== m2) {
        pairs.push([m1, m2]);
      }
    });
  });
  return pairs;
}

function attempt() {
  const moons = [
    { x: 13, y: -13, z: -2, vx: 0, vy: 0, vz: 0 },
    { x: 16, y: 2, z: -15, vx: 0, vy: 0, vz: 0 },
    { x: 7, y: -18, z: -12, vx: 0, vy: 0, vz: 0 },
    { x: -3, y: -8, z: -8, vx: 0, vy: 0, vz: 0 },
  ];
  const testMoons = [
    { x: -1, y: 0, z: 2, vx: 0, vy: 0, vz: 0 },
    { x: 2, y: -10, z: -7, vx: 0, vy: 0, vz: 0 },
    { x: 4, y: -8, z: 8, vx: 0, vy: 0, vz: 0 },
    { x: 3, y: 5, z: -1, vx: 0, vy: 0, vz: 0 },
  ];
  const testMoons2 = [
    { x: -8, y: -10, z: 0, vx: 0, vy: 0, vz: 0 },
    { x: 5, y: 5, z: 10, vx: 0, vy: 0, vz: 0 },
    { x: 2, y: -7, z: 3, vx: 0, vy: 0, vz: 0 },
    { x: 9, y: -8, z: -3, vx: 0, vy: 0, vz: 0 },
  ];

  // energyAfterTicks(moons, 1000);
  console.log(ticksToRepeat(testMoons2));
}

export default function() {
  // calculated in julia
  const xStep = 286332;
  const yStep = 161428;
  const zStep = 102356;

  console.log(lcm(lcm(286332, 161428), 102356));
}
