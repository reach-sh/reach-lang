import { loadStdlib } from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';
import { getTestNetAccount } from './util.mjs';

const getTokenInfo = async () => {
  const tokSym = await ask.ask(`Token symbol:`);
  const tokName = await ask.ask(`Token name:`);
  return [ tokSym, tokName ];
}

export const runTokens = async (useTestnet) => {
  const stdlib = await loadStdlib();

  let accCreator;
  if (useTestnet) {
    stdlib.setProviderByName(`TestNet`);
    accCreator = await getTestNetAccount(stdlib);
  } else {
    const startingBalance = stdlib.parseCurrency(100);
    accCreator = await stdlib.newTestAccount(startingBalance);
  }

  if (stdlib.connector == 'ETH') {
    accCreator.setGasLimit(5000000);
  }

  console.log(`Creating first token...`);
  const [symA, nameA] = await getTokenInfo();

  console.log(`Creating second token...`);
  const [symB, nameB] = await getTokenInfo();

  const tokA = await launchToken(stdlib, accCreator, nameA, symA);
  const tokB = await launchToken(stdlib, accCreator, nameB, symB);
  console.log(`Token Info:`, JSON.stringify({
    tokA: tokA.id,
    tokB: tokB.id,
  }));

  while (true) {
    console.log(`Ready To Mint 1000 ${symA} & 1000 ${symB}`);
    const addr = await ask.ask(`Address: `);
    const acc = (stdlib.connector == 'ALGO')
      ? { networkAccount: { addr } }
      : { networkAccount: { address: addr } };
    await tokA.mint(acc, stdlib.parseCurrency(1000));
    await tokB.mint(acc, stdlib.parseCurrency(1000));
  }
}
