import { loadStdlib } from "@reach-sh/stdlib";
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib();

const sbal = stdlib.parseCurrency(100);
const acc = await stdlib.newTestAccount(sbal);

const supply = '18446744073709551615';

const tokA = await stdlib.launchToken(acc, "zorkmid", "ZMD", { supply });

const assertEq = (a, e) => {
  if (!a.eq(e)) {
    throw Error(`Expected ${e}, got ${a}`);
  }
};

{ // sync indexer with latest blocks, so it can observe token metadata
  const ctc = acc.contract(backend);
  await ctc.p.Alice({});
  await ctc.e.sync.next();
}

const mdA = await acc.tokenMetadata(tokA.id);

assertEq( mdA.supply, supply );

assertEq( await stdlib.balanceOf(acc, tokA.id), supply );
