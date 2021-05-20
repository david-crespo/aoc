type Point = { x: number; y: number };
type Point3D = { x: number; y: number; z: number };

type Board = number[][];
type Bord = string[][];
type Boord = boolean[][];

type Count = { [k: string]: number };

type Graph = {
  addNode: (k: string, edges: Count) => void;
  path: (a: string, b: string, options?: { avoid?: string[] }) => string[];
  removeNode: (k: string) => void;
  graph: Map<string, Map<string, number>>;
};
