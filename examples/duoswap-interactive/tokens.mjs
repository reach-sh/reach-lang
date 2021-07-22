import { loadStdlib } from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import * as ask from '@reach-sh/stdlib/ask.mjs';

export const runTokens = async () => {
  const stdlib = await loadStdlib();
  const zmd = await launchToken("zorkmid", "ZMD");
  const gil = await launchToken("gil", "GIL");
  console.log(`Token Info:`, JSON.stringify({ zmd: zmd.id, gil: gil.id }));

  while (true) {
    console.log(`Ready To Mint 1000 ZMD & 1000 GIL`);
    const addr = await ask.ask(`Address: `);
    const acc = { networkAccount: { address: addr } };
    zmd.mint(acc, stdlib.parseCurrency(1000));
    gil.mint(acc, stdlib.parseCurrency(1000));
  }
}
