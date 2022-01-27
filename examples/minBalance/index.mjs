import { loadStdlib } from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import * as backend from './build/index.main.mjs'
import assert from 'assert';

const [, , infile] = process.argv;

(async () => {

    const stdlib = await loadStdlib();
    const startingBalance = stdlib.parseCurrency(100);

    const getBalance = async (who, tok) =>
        stdlib.formatCurrency(await stdlib.balanceOf(who, tok), 4);

    const getMinBalance = async (who) =>
        stdlib.formatCurrency(await stdlib.minBalance(who))

    // Alice mints a token (asset)
    // Bob opts in to token (asset)
    // Eve creates an app (app extra pages and global state)

    const accAlice = await stdlib.newTestAccount(startingBalance);
    const accBob = await stdlib.newTestAccount(startingBalance);
    const accEve = await stdlib.newTestAccount(startingBalance);

    const beforeAlice = await getBalance(accAlice);
    const beforeBob = await getBalance(accBob);
    const beforeAliceMin = await getMinBalance(accAlice);
    const beforeBobMin = await getMinBalance(accBob);
    const beforeEveMin = await getMinBalance(accEve);

    const zorkmid = await launchToken(stdlib, accAlice, "zorkmid", "ZMD", { decimals: 6, supply: 1000000000 });
    await accAlice.tokenAccept(zorkmid.id)
    await accBob.tokenAccept(zorkmid.id)

    const ctc = accAlice.contract(backend)
    Promise.all([backend.Alice(ctc, {})])
    const appId = await ctc.getInfo()
    console.log({ appId })
    const ctc2 = accBob.contract(backend, appId)
    backend.Bob(ctc2, {})

    const afterAlice = await getBalance(accAlice);
    const afterBob = await getBalance(accBob);
    const afterAliceMin = await getMinBalance(accAlice);
    const afterBobMin = await getMinBalance(accBob);
    const afterEveMin = await getMinBalance(accEve);

    console.log({ beforeAlice, beforeAliceMin, beforeBobMin, beforeBob, beforeEveMin })
    console.log({ afterAlice, afterAliceMin, afterBobMin, afterBob, afterEveMin })

    assert(beforeBobMin === beforeAliceMin)
    assert(beforeBobMin === beforeEveMin)
    assert(beforeBobMin === '0.1')

    assert(afterAliceMin === '0.4')
    assert(afterBobMin === '0.2')
    assert(afterEveMin === '0.1')

    process.exit()

})();