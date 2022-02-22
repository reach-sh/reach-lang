import {loadStdlib} from '@reach-sh/stdlib';
import * as backend from './build/index.main.mjs';
const stdlib = loadStdlib(process.env);

const startingBalance = stdlib.parseCurrency(100);
const [accAlice, accCreator] =
  await stdlib.newTestAccounts(2, startingBalance);

const neosatoshi = await stdlib.launchToken(accCreator, "neosatoshi", "NEO");
await accAlice.tokenAccept(neosatoshi.id);

const fmt = (amt) => `${stdlib.formatWithDecimals(amt, 0)} NEO = `
                   + `${stdlib.formatWithDecimals(amt, 3)} kilo-NEO = `
                   + `${stdlib.formatWithDecimals(amt, 6)} mega-NEO`

const mintNeosatoshi = async (amt) => {
  await neosatoshi.mint(accAlice, amt);
  console.log(`Minting ${fmt(amt)}`);
  console.log(`Alice now has ${fmt(await stdlib.balanceOf(accAlice, neosatoshi.id))}`);
}

await mintNeosatoshi(1);
await mintNeosatoshi(50);
await mintNeosatoshi(500000000);
await mintNeosatoshi(100);
