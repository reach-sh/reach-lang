import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const timeoutK = stdlib.connector === 'ALGO' ? 1 : 3;
  const startingBalance = stdlib.parseCurrency(10);
  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));

  const accAlice  = await stdlib.newTestAccount(startingBalance);
  const accBob    = await stdlib.newTestAccount(startingBalance);
  const accClaire = await stdlib.newTestAccount(startingBalance);

  const ctcAlice = accAlice.deploy(backend);

  const everyone = [
    [' Alice', accAlice],
    ['   Bob', accBob],
    ['Claire', accClaire],
  ];
  const randomArrayRef = (arr) =>
    arr[Math.floor(Math.random() * arr.length)];

  const auctionProps = {
    ' Alice': {
      startingBid: stdlib.parseCurrency(0),
      timeout: timeoutK * 3,
    },
    '   Bob': {
      startingBid: stdlib.parseCurrency(1),
      timeout: timeoutK * 3,
    },
    'Claire': {
      startingBid: stdlib.parseCurrency(3),
      timeout: timeoutK * 4,
    }
  };

  const bids = {
    ' Alice': {
      maxBid: stdlib.parseCurrency(7),
    },
    '   Bob': {
      maxBid: stdlib.parseCurrency(40),
    },
    'Claire': {
      maxBid: stdlib.parseCurrency(20),
    }
  };

  const trades = {
    ' Alice': 0, '   Bob': 0, 'Claire': 0
  };

  const makeOwner = (acc, who) => {
    const ctc = acc.attach(backend, ctcAlice.getInfo());
    const others = everyone.filter(x => x[0] !== who);
    return backend.Owner(ctc, {
      showOwner: ((id, owner) => {
        if ( stdlib.addressEq(owner, acc) ) {
          console.log(`\n${who} owns it\n`);
          if ( trades[who] == 2 ) {
            console.log(`${who} stops`);
            process.exit(0);
          } else {
            trades[who] += 1;
          }
        }
      }),
      getAuctionProps: (() => {
        console.log(`${who} starts the bidding at ${fmt(auctionProps[who].startingBid)}`);
        return auctionProps[who];
      }),
      getBid: (price) => {
        if (price < bids[who].maxBid) {
          const bid = stdlib.add(price, stdlib.parseCurrency(1));
          console.log(`${who} tries to bid ${fmt(bid)} (based on price: ${fmt(price)})`);
          return ['Some', bid];
        } else {
          return ['None', null];
        }
      },
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
