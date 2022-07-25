import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const accAlice = await stdlib.newTestAccount(startingBalance);
const accBob = await stdlib.newTestAccount(startingBalance);



const ctcAlice = accAlice.contract(backend);
const ctcBob = accBob.contract(backend, ctcAlice.getInfo());

const algoP = stdlib.connector === "ALGO";
let algoAlreadyAccepted = false;

const Player = (acc) => ({
  seeTokenAccepted: async (t, acceptedP) => {
    const localAcceptedP = await acc.tokenAccepted(t);
    const matched = acceptedP === localAcceptedP;
    if (! matched) {
      throw "Result for tokenAccepted in consensus and locally didn't match."
    }
    if (algoP && !acceptedP && !algoAlreadyAccepted) {
      await acc.tokenAccept(t);
      algoAlreadyAccepted = true;
      const localAfterAccept = await acc.tokenAccepted(t);
      if (! localAfterAccept) {
        throw "acc.tokenAccept failed"
      }
    }
  },
})

await Promise.all([
  ctcAlice.p.Alice({
    ...Player(accAlice),
  }),
  ctcBob.p.Bob({
    ...Player(accBob),
  }),
]);


