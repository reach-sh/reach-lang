import * as loader from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loader.loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);
  const accClaire = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);

  const accEve = await stdlib.newTestAccount(startingBalance);
  const ctcEve = accEve.attach(backend, ctcAlice.getInfo());
  const externalViewer = async () => {
    console.log(`Eve sees who the owner is...`);
    const owner = await ctcEve.getViews().NFT.owner();
    console.log(`...it is ${stdlib.formatAddress(owner[1])}`);
  };

  const everyone = [
    [' Alice', accAlice],
    ['   Bob', accBob],
    ['Claire', accClaire],
  ];
  const randomArrayRef = (arr) =>
    arr[Math.floor(Math.random() * arr.length)];

  let trades = 3;
  const makeOwner = (acc, who) => {
    const ctc = acc.attach(backend, ctcAlice.getInfo());
    const others = everyone.filter(x => x[0] !== who);
    return backend.Owner(ctc, {
      newOwner: (async () => {
        await externalViewer();
        if ( trades == 0 ) {
          console.log(`${who} stops`);
          process.exit(0);
        }
        const next = randomArrayRef(others);
        console.log(`${who} trades to ${next[0]}`);
        trades--;
        return next[1];
      }),
      showOwner: ((id, owner) => {
        if ( stdlib.addressEq(owner, acc) ) {
          console.log(`${who} sees that they own it`);
        } else {
          console.log(`${who} sees that ${stdlib.formatAddress(owner)} owns it`);
        }
      }),
    });
  };

  await Promise.all([
    backend.Creator(
      ctcAlice,
      { getId: () => {
        const id = stdlib.randomUInt();
        console.log(` Alice makes id #${id}`);
        return id; }
      },
    ),
    makeOwner(accAlice , ' Alice'),
    makeOwner(accBob   , '   Bob'),
    makeOwner(accClaire, 'Claire'),
  ]);
})();
