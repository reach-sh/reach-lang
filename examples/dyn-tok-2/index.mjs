import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);
import { util } from '@reach-sh/stdlib';
const { thread, Signal } = util;

(async () => {
  if (stdlib.connector !== 'ALGO') {
    // ETH API calls for some reason and never halts
    // CFX can't compare tokens?
    return;
  }
  const startingBalance = stdlib.parseCurrency(100);

  const accounts =
    await stdlib.newTestAccounts(3, startingBalance);

  const [ accAlice, accBobby, accCamus ] = accounts;
  const ready = new Signal();

  const ctcAlice = accAlice.contract(backend);
  const tok1 = await stdlib.launchToken(accAlice, 'ZMD', 'ZMD');
  const tok2 = await stdlib.launchToken(accAlice, 'GIL', 'GIL');

  const setup = async (acc) => {
    await acc.tokenAccept(tok1.id);
    await acc.tokenAccept(tok2.id);
    await tok1.mint(acc, startingBalance);
    await tok2.mint(acc, startingBalance);
    if (stdlib.connector == 'ETH') {
      acc.setGasLimit(8000000)
    }
  }
  await setup(accAlice);
  await setup(accBobby);
  await setup(accCamus);

  const getBals = async (acc) => [
    await stdlib.balanceOf(acc, tok1.id),
    await stdlib.balanceOf(acc, tok2.id),
  ]

  const amt = 100000;

  const user = async (acc) => {
    const ctc = acc.contract(backend, ctcAlice.getInfo());
    const v = ctc.v.Bals;
    const getTokBals = async () => {
      return [ (await v.tok1())[1], (await v.tok2())[1] ];
    };
    const getCurrentTok = async () => {
      return (await v.currentTok())[1];
    }
    await ready.wait();
    const call = async (tok) => {
      console.log('Calling changeTok with: ', tok);
      const currentTok = await getCurrentTok();
      const ogBals = await getTokBals();
      try {
        await ctc.a.B.changeTok(tok);
        const actBals = await getTokBals();
        const expBals = stdlib.tokenEq(currentTok, tok1.id)
          ? [ ogBals[0].add(amt), ogBals[1] ]
          : [ ogBals[0], ogBals[1].add(amt) ];

        stdlib.assert(JSON.stringify(actBals) == JSON.stringify(expBals))
      } catch (e) {
        console.log(e);
      }
    }
    await call(tok2.id);
    await call(tok1.id);
  }

  let done = false;
  await Promise.all([
    thread(async () => {
      await user(accBobby);
      await user(accCamus);
      done = true;
    }),
    backend.A(ctcAlice, {
      tok1: tok1.id,
      tok2: tok2.id,
      amt,
      log: (...args) => {
        console.log(...args);
        ready.notify();
      },
      stop: () => {
        if (done) { console.log("Stopping"); }
        return done;
      },
    }),
  ]);
})();
