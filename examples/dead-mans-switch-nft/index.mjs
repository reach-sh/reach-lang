import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const [accCreator, accAlice, accBob] =
  await stdlib.newTestAccounts(3, stdlib.parseCurrency(100));

const ctcCreator = accCreator.contract(backend);
const ctcAlice = accAlice.contract(backend, ctcCreator.getInfo());
const ctcBob = accBob.contract(backend, ctcCreator.getInfo());

const runOwner = async (ctc, name, maxRenews) => {
  let renews = 0;
  await backend.Owner(ctc, {
    notifyReceive: () => console.log(`${name} received the token`),
    notifyRenewFail: () => console.log(`${name} lost the token`),
    notifyRenewSuccess: () => console.log(`${name} renews the token`),
    notifyRenewNeeded: async () => {
      console.log(`${name} needs to renew the token`);
      if (++renews >= maxRenews) {
        console.log(`${name} is now dead and cannot renew the token!`);
        await stdlib.wait(10000);
      }
    },
  });
};

let nextReceiver = 'Alice';

await Promise.all([
  backend.Creator(ctcCreator, {
    transferTo: () => {
      console.log(`Creator transferring token to ${nextReceiver}`);
      switch (nextReceiver) {
        case 'Alice':
          nextReceiver = 'Bob';
          return accAlice.getAddress();
        case 'Bob':
          nextReceiver = 'nobody';
          return accBob.getAddress();
        default:
          process.exit(0);
      }
    }
  }),
  runOwner(ctcAlice, 'Alice', 2),
  runOwner(ctcBob, 'Bob', 3),
]);
