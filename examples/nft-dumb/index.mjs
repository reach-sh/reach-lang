import * as loader from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const startingBalance = stdlib.parseCurrency(100);

  const [ accAlice, accBob, accClaire, accEve ] = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.contract(backend);

  const user = async(uid) => {
    const acc = await stdlib.newTestAccount(startingBalance);
    acc.setDebugLabel(uid);
    return async () => {
      const ctcEve = acc.contract(backend, ctcAlice.getInfo());
      
      const call = async (f) => {
        let res = undefined;
        try {
          res = await f();
        } catch (e) {
          res = [`err`, e]
        }
        console.log(`res`, res);
      }

      await call(() => ctcEve.Owner.newOwner(accEve.getInfo()));
    }
  }

  const externalViewer = async () => {
    console.log(`Eve sees who the owner is...`);
    const owner = await ctcEve.v.NFT.owner();
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
    const ctc = acc.contract(backend, ctcAlice.getInfo());
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

  await call(() => ctcAlice.Owner.makeOwner(accAlice , ' Alice'));
  await call(() => ctcAlice.Owner.showOwner(accBob   , '   Bob'));
  await call(() => ctcAlice.Owner.showOwner(accClaire, 'Claire'));

  await Promise.all([
    backend.Creator(
      ctcAlice,
      { getId: () => {
        const id = stdlib.randomUInt();
        console.log(` Alice makes id #${id}`);
        return id; }
      },
    ),
  ]);
})();
