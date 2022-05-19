import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);

const [ accCreator, accAlice ] = await stdlib.newTestAccounts(2, stdlib.parseCurrency(100));

const egg = await stdlib.launchToken(accCreator, "Eggs",  "EGG");
const bkn = await stdlib.launchToken(accCreator, "Bacon", "BKN");
const tst = await stdlib.launchToken(accCreator, "Toast", "TST");

await accAlice.tokenAccept(egg.id);
await accAlice.tokenAccept(bkn.id);
await accAlice.tokenAccept(tst.id);

await egg.mint(accAlice, 50);
await bkn.mint(accAlice, 100);
await tst.mint(accAlice, 150);

stdlib.assert((await accAlice.balanceOf(egg.id)).eq(50), 'balanceOf egg == 50');
stdlib.assert((await accAlice.balanceOf(bkn.id)).eq(100), 'balanceOf bkn == 100');
stdlib.assert((await accAlice.balanceOf(tst.id)).eq(150), 'balanceOf tst == 150');

const [eggBal, bknBal, tstBal] = await accAlice.balancesOf([egg.id, bkn.id, tst.id]);

stdlib.assert(eggBal.eq(50), 'balancesOf[egg] = 50');
stdlib.assert(bknBal.eq(100), 'balancesOf[bkn] = 100');
stdlib.assert(tstBal.eq(150), 'balancesOf[tst] = 150');
