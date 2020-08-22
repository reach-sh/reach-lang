import * as stdlib from '@reach-sh/stdlib/ETH.mjs';
import * as backend from './build/tut.main.mjs';

( async () => {
  const toNetworkFormat = (n) => stdlib.toWeiBigNumber(n, 'ether');

  const accAlice = await stdlib.newTestAccount(toNetworkFormat('10'));
  const accBob = await stdlib.newTestAccount(toNetworkFormat('10'));

  const ctcAlice = await accAlice.deploy(backend);
  const ctcBob = await accBob.attach(backend, ctcAlice);

  await Promise.all([
    backend.Alice(
      stdlib, ctcAlice,
      {}),
    backend.Bob(
      stdlib, ctcBob,
      {})
  ]);
})();
