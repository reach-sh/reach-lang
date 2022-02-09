import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

const stdlib = loadStdlib(process.env);

// Make accounts
const startingBalance = stdlib.parseCurrency(100);
const [ accAlice, accBob, accCreator ] =
  await stdlib.newTestAccounts(3, startingBalance);

// Create tokens make Alice and Bob's accounts accept them
const chicken = await stdlib.launchToken(accCreator, "chicken", "CKN");
const egg = await stdlib.launchToken(accCreator, "egg", "EGG");
await accAlice.tokenAccept(chicken.id);
await accAlice.tokenAccept(egg.id);
await accBob.tokenAccept(chicken.id);
await accBob.tokenAccept(egg.id);

// Give Alice some CKN and EGG tokens
const aliceStartingCkn = stdlib.parseCurrency(200);
const aliceStartingEgg = stdlib.parseCurrency(300);
await chicken.mint(accAlice, aliceStartingCkn);
await egg.mint(accAlice, aliceStartingEgg);

const fmt = (x) => stdlib.formatCurrency(x, 4);
const doDonation = async (donatedCkn, donatedEgg) => {
  const balancesMessage = async (account) => {
    const [nwt, ckn, egg_] = await stdlib.balancesOf(account, [null, chicken.id, egg.id]);
    return `${fmt(nwt)} ${stdlib.standardUnit}, ${fmt(ckn)} CKN, ${fmt(egg_)} EGG`;
  };

  console.log('\n');
  console.log(`Alice has ${await balancesMessage(accAlice)}`);
  console.log(`  Bob has ${await balancesMessage(accBob)}`);

  // Alice launches the donation contract, and Bob joins
  const ctcAlice = accAlice.contract(backend);
  const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

  // Alice and Bob provide frontend interfaces so the contract can take place
  await Promise.all([
    backend.Alice(ctcAlice, {
      getTokenIds: () => { return [chicken.id, egg.id]; },
      getDonation: () => {
        console.log(`Alice wants to donate ${fmt(donatedCkn)} CKN and ${fmt(donatedEgg)} EGG`);
        return [donatedCkn, donatedEgg];
      },
    }),

    backend.Bob(ctcBob, {
      showDonation: (recvCkn, recvEgg) => {
        console.log(`Bob sees that he will receive ${fmt(recvCkn)} CKN and ${fmt(recvEgg)} EGG`);
      }
    }),
  ]);

  console.log(`Alice now has ${await balancesMessage(accAlice)}`);
  console.log(`  Bob now has ${await balancesMessage(accBob)}`);
};

await doDonation(stdlib.parseCurrency(1), stdlib.parseCurrency(2));
await doDonation(stdlib.parseCurrency(5), stdlib.parseCurrency(10));
await doDonation(stdlib.parseCurrency(100), stdlib.parseCurrency(200));
