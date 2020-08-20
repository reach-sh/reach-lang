import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as App from './build/over-minimal.main.mjs';

( async () => {
  const etherToWei = (n) => stdlib.toWeiBigNumber(n, 'ether');

  const accAlice = await stdlib.newTestAccount(etherToWei('5'));
  const accBob = await stdlib.newTestAccount(etherToWei('10'));

  const ctcAlice = await accAlice.deploy(App);
  const ctcBob = await accBob.attach(App, ctcAlice);

  await Promise.all([
    App.Alice(
      stdlib, ctcAlice,
      { request: etherToWei('5'),
        info: stdlib.toHex('If you wear the Holy Glasses, you can see beyond evil illusions.') }),
    App.Bob(
      stdlib, ctcBob,
      { want: (amt) => console.log(`Alice asked Bob for ${amt}`),
        got: (secret) => console.log(`Alice's secret is: ${stdlib.hexToString(secret)}`) })
  ]);
})();
