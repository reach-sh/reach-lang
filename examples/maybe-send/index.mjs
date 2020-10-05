import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

const demo = async (x) => {
  const stdlib = await stdlib_loader.loadStdlib();

  const startingBalance = stdlib.parseCurrency(100);
  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  console.log(`Alice will deploy the contract.`);
  const ctcAlice = alice.deploy(backend);

  console.log(`Bob will attach to the contract.`);
  const ctcBob = bob.attach(backend, ctcAlice.getInfo());

  const showThing = (showLabel) => (mx) => {
    console.log(`Bob.${showLabel}`);
    const [label, val] = mx;
    switch (label) {
    case 'Some':
      console.log(`Maybe(UInt256).Some(${val})`);
      if (stdlib.isBigNumber(val)) {
        console.log(`  where ${val} is a BigNumber`);
      } else {
        console.log(`  where ${val} is not a BigNumber`);
      }
      break;
    case 'None':
      console.log(`Maybe(UInt256).None()`);
      break;
    default:
      console.log(`Unexected: ${mx}`);
      break;
    }
  };

  console.log(`Both will play their parts.`);
  await Promise.all([
    backend.Alice(stdlib, ctcAlice, {
      ...stdlib.hasRandom,
      getX: () => {
        return x || 0;
      },
      getMx: () => {
        console.log(`Alice.getMx`);
        if (x) {
          return ['Some', x];
        } else {
          return ['None', null];
        }
      },
    }),
    backend.Bob(stdlib, ctcBob, {
      ...stdlib.hasRandom,
      showMx: showThing('showMx'),
      showMy: showThing('showMy'),
    }),
  ]);

  console.log('Alice and Bob are done.');
};

(async () => {
  await demo(1);
  await demo(null);
})();
