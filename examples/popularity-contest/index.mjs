import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const [ N, timeoutFactor ] =
    stdlib.standardUnit === 'ALGO' ? [ 5, 2 ] : [ 20, 6 ];

  const startingBalance = stdlib.parseCurrency(10);
  const accPollster = await stdlib.newTestAccount(startingBalance);
  const accVoter_arr = await Promise.all( Array.from({length: N}, () => stdlib.newTestAccount(startingBalance)) );
  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  const fmt = (x) => stdlib.formatCurrency(x, 4);
  const getBalance = async (who) => fmt(await stdlib.balanceOf(who));
  const beforeAlice = await getBalance(accAlice);
  const beforeBob = await getBalance(accBob);

  const ctcPollster = accPollster.deploy(backend);
  const ctcInfo = ctcPollster.getInfo();

  const OUTCOME = ['Alice wins', 'Bob wins', 'Timeout'];
  const Common = (Who) => ({
      showOutcome: (outcome, forA, forB) => {
        if ( outcome == 2 ) {
          console.log(`${Who} saw the timeout`); }
        else {
          console.log(`${Who} saw a ${forA}-${forB} outcome: ${OUTCOME[outcome]}`);
        }
  } });

  await Promise.all([
    backend.Pollster(ctcPollster, {
      ...Common('Pollster'),
      getParams: () => ({
        ticketPrice: stdlib.parseCurrency(5),
        deadline: N*timeoutFactor,
        aliceAddr: accAlice,
        bobAddr: accBob,
      }),
    }),
  ].concat(
    accVoter_arr.map((accVoter, i) => {
      const ctcVoter = accVoter.attach(backend, ctcInfo);
      const Who = `Voter #${i}`;
      const vote = Math.random() < 0.5;
      let voted = false;
      return backend.Voter(ctcVoter, {
        ...Common(Who),
        getVote: (() => vote),
        voterWas: ((voterAddr) => {
          if ( stdlib.addressEq(voterAddr, accVoter) ) {
            console.log(`${Who} voted: ${vote ? 'Alice' : 'Bob'}`);
            voted = true;
          } } ),
        shouldVote: (() => ! voted) }); } )
  ));

  const afterAlice = await getBalance(accAlice);
  const afterBob = await getBalance(accBob);

  console.log(`Alice went from ${beforeAlice} to ${afterAlice}.`);
  console.log(`Bob went from ${beforeBob} to ${afterBob}.`);

})();
