import { loadStdlib } from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';

(async () => {
  const stdlib = await loadStdlib();
  const startingBalance = stdlib.parseCurrency(10);

  const accAlice = await stdlib.newTestAccount(startingBalance);
  const accBob = await stdlib.newTestAccount(startingBalance);

  console.log(`accAlice is using stdlib: ${accAlice.stdlibT}`);
  console.log(`accBob is using stdlib: ${accBob.stdlibT}`);

  const ctcAlice = accAlice.deploy(backend);
  const ctcBob = accBob.attach(backend, ctcAlice.getInfo());

  console.log(`ctcAlice is using stdlib: ${ctcAlice.stdlibT}`);
  console.log(`ctcBob is using stdlib: ${ctcBob.stdlibT}`);


  await Promise.all([
    backend.Alice(
      ctcAlice,
      {
        x: 5
      },
    ),
    backend.Bob(
      ctcBob,
      {
        x: 10
      },
    ),
  ]);

  console.log('Done');
})(); // <-- Don't forget these!
