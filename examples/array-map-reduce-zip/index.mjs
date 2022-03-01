import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accA = await stdlib.newTestAccount(startingBalance);
const accB = await stdlib.newTestAccount(startingBalance);
const ctcA = accA.contract(backend);
const ctcB = accB.contract(backend, ctcA.getInfo());

const expect = (expected, actual, m) => {
  if (JSON.stringify(expected) != JSON.stringify(actual)) {
    console.log(m + ": expected: " + expected + ", actual: " + actual);
    throw m;
  }
  return;
}

const num = stdlib.bigNumberToNumber;

const Player = {
  ...stdlib.hasConsoleLogger,
  getArray: () => {
    return [0, 1, 2, 3, 4];
  },
  expectMapped: (arr) => expect([0,2,0,6,0], arr.map(num), "local mapped value"),
  expectReduced: (v) => expect(260, num(v), "local reduced value"),
  expectConsensusMapped: (arr) => expect([0,3,2,9,4], arr.map(num), "consensus mapped value"),
  expectConsensusReduced: (v) => expect(28, num(v), "consensus mapped value"),
  expectConsensusZipped: (arr) => expect([[0,0,1], [2,1,1], [0,2,1], [6,3,1], [0,4,1]],
                                         arr.map((ia)=>ia.map(num)),
                                         "consensus zipped value"),

};

await Promise.all([
  ctcA.p.A({...Player,}),
  ctcB.p.B({...Player,}),
]);
