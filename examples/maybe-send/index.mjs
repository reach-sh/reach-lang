import * as stdlib_loader from '@reach-sh/stdlib/loader.mjs';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await stdlib_loader.loadStdlib();
  const connector = await stdlib_loader.getConnector();
  // TODO: expose data constructors via backend, incl user-defined
  // const {Some, None} = backend._DataConstructors;
  const {Some, None} = stdlib;

  const startingBalance =
        connector == 'ETH' ? stdlib.toWeiBigNumber('100', 'ether') :
        1000000;
  const alice = await stdlib.newTestAccount(startingBalance);
  const bob = await stdlib.newTestAccount(startingBalance);

  console.log(`Alice will deploy the contract.`);
  const ctcAlice = await alice.deploy(backend);

  console.log(`Bob will attach to the contract.`);
  const ctcBob = await bob.attach(backend, ctcAlice);

  console.log(`Both will play their parts.`);
  await Promise.all([
    backend.Alice(stdlib, ctcAlice, {
      ...stdlib.hasRandom,
      getMx: () => {
        console.log(`Alice.getMx`);
        const x = Math.floor(Math.random() * 10);
        if (x < 5) {
          return None();
        } else {
          return Some(x);
        }
      },
    }),
    backend.Bob(
      stdlib, ctcBob,
      {
        ...stdlib.hasRandom,
        showMx: (mx) => {
          console.log(`Bob.showMx`);
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
        }
      }
    ),
  ]);

  console.log('Alice and Bob are done.');
})();
