import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const sleepSecs = async (x) => {
  await new Promise(res => setTimeout(res, x * 1000));
};

(async () => {
  const startingBalance = stdlib.parseCurrency(100);
  const [accAlice, accBob] =
    await stdlib.newTestAccounts(2, startingBalance);
  accAlice.setDebugLabel('Alice');
  accBob.setDebugLabel('Bob');
  console.log('Launching...');
  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());
  const lctView = ctcAlice.v.LCT;
  console.log('Starting backends...');
  const commonBackend = (who) => ({
    isObserving: async (lct) => {
      const mLatestLCT = await lctView.current();
      const latestLCT = mLatestLCT[0] === 'None' ? 0 : mLatestLCT[1];
      console.log(`${who}'s LCT is ${lct.toString()}'`);
      console.log(`Latest LCT is ${latestLCT.toString()}'`);
      return lct > latestLCT;
    },
    log: console.log,
  });
  await Promise.all([
    backend.Alice(ctcAlice, {
      ...commonBackend("Alice"),
      doAliceStuff: async () => {
        console.log("Doing Alice stuff");
      }
    }),
    backend.Bob(ctcBob, {
      ...commonBackend("Bob"),
      doBobStuff: async () => {
        await sleepSecs(2);
        console.log("Doing Bob stuff");
      }
    }),
  ]);
})();
