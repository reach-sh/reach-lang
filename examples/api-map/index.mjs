import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
const thread = async (f) => await f();
export class Signal {
  constructor() {
    const me = this;
    this.p = new Promise((resolve) => { me.r = resolve; });
  }
  wait() { return this.p; }
  notify() { this.r(true); }
};

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accA, accB, accC ] =
    await stdlib.newTestAccounts(3, startingBalance);
  accA.setDebugLabel('A');
  accB.setDebugLabel('B');
  accC.setDebugLabel('C');

  const ctcA = accA.contract(backend);
  const ctcB = accB.contract(backend, ctcA.getInfo());
  const ctcC = accC.contract(backend, ctcA.getInfo());
  const ready = new Signal();

  const LOG = (...args) => {
    console.log("");
    console.log("");
    console.log("");
    console.log(...args);
    console.log("");
    console.log("");
    console.log("");
  };

  const aP = ctcA.p.D({
    ready: () => {
      LOG(`A says its ready`);
      ready.notify();
    },
    log: (...args) => LOG(`A sees`, ...args),
  });

  await ready.wait();

  await ctcB.a.P.put(10);
  await ctcA.a.P.done();
  await ctcC.a.P.put(10);
  await ctcB.a.P.put(10);
  await ctcB.a.P.get(20);
  await ctcC.a.P.get(10);

  await aP;
})();
