import { readIntCodes } from "./util";
import { IntCodeMachine } from "./intCodes";
import { takeWhile, remove } from "lodash";
import * as fs from "fs";
const readline = require("readline-sync");

type Room = {
  north?: string;
  east?: string;
  south?: string;
  west?: string;
  inv: Set<string>;
};

type ShipMap = { [k: string]: Room };

type Dir = "north" | "south" | "east" | "west";

function getRoom(output: string): string | undefined {
  const re = output.match(/== ([A-Za-z ]+) ==/);
  return re ? re[1] : undefined;
}

function updateMap(
  map: ShipMap,
  command: Dir | null,
  prevRoom: string,
  currRoom: string,
  inv?: string[],
) {
  if (!map[prevRoom]) {
    map[prevRoom] = { inv: new Set() };
  }
  if (command) {
    map[prevRoom][command] = currRoom;
  }
  if (!map[currRoom]) {
    map[currRoom] = { inv: new Set() };
  }
  if (inv) {
    map[currRoom].inv = new Set(inv);
  }
}

const getItems = (output: string) => {
  const lines = output.trim().split("\n");
  const itemsIdx = lines.indexOf("Items here:");
  return itemsIdx >= 0
    ? takeWhile(lines.slice(itemsIdx + 1), l => l.startsWith("- ")).map(l =>
        l.substring(2).trim(),
      )
    : [];
};

const isDir = (s: string): s is Dir => /^(north|south|east|west)$/.test(s);

const dotEdge = (name: string, room: Room, dir: Dir) =>
  room[dir]
    ? `"${name}" -> "${room[dir]}" [label=${dir[0].toUpperCase()}]`
    : null;

function invStr(inv: Set<string>): string {
  if (inv.size === 0) return "";
  return `\\n(${Array.from(inv).join(", ")})`;
}

// dot src/day25.dot -Tpng:quartz:quartz -o ship.png

function writeMapToFile(map: ShipMap) {
  let dot = Object.entries(map)
    .map(([name, room]) =>
      [
        `"${name}" [label="${name}${invStr(room.inv)}"]`,
        dotEdge(name, room, "north"),
        dotEdge(name, room, "south"),
        dotEdge(name, room, "east"),
        dotEdge(name, room, "west"),
      ]
        .filter(x => x)
        .join("\n"),
    )
    .join("\n");
  dot = `digraph {\nnode [shape=record];\n${dot}\n}`;
  fs.writeFileSync("src/day25.dot", dot);
}

const pressureReject = (output: string) =>
  /== Pressure-Sensitive Floor ==/.test(output) &&
  /Alert!/.test(output) &&
  /You can't go that way/.test(output);

// don't take: photons, giant electromagnet, molten lava, escape pod, infinite loop

const steps = [
  "east",
  "east",
  "east",
  // "take shell",
  "west",
  "south",
  "take monolith",
  "north",
  "west",
  "north",
  "west",
  // "take bowl of rice",
  "east",
  "north",
  "take planetoid",
  "west",
  // "take ornament",
  "west",
  "east",
  "south",
  "south",
  "take fuel cell",
  "north",
  "north",
  "east",
  "east",
  // "take cake",
  "north",
  "south",
  "south",
  "west",
  "west",
  "east",
  "north",
  "take astrolabe",
  "west",
  "north",
];

export default function() {
  const intCodes = readIntCodes("day25-input.txt");
  const machine = new IntCodeMachine(intCodes);

  const map: ShipMap = {};
  const myInv = new Set<string>();
  let prevRoom: string = "";
  let currRoom: string = "Hull Breach";

  console.log(machine.runAscii(""));
  while (true) {
    const command: string = steps.shift() || readline.question("");
    prevRoom = currRoom;
    const output = machine.runAscii(command + "\n");
    if (typeof output === "string" && !pressureReject(output)) {
      const room = getRoom(output);
      if (room && isDir(command)) {
        currRoom = room;
        const inv = getItems(output);
        updateMap(map, command, prevRoom, currRoom, inv);
        writeMapToFile(map);
      }
    }
    console.log(output);
  }
}
