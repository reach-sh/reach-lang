import { loadStdlib } from "@reach-sh/stdlib";
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

const mdA = await acc.tokenMetadata(tokA.id);

assertEq( mdA.supply, supply );

assertEq( await stdlib.balanceOf(acc, tokA.id), supply );
