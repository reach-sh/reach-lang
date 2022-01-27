import { loadStdlib } from '@reach-sh/stdlib';
import launchToken from '@reach-sh/stdlib/launchToken.mjs';
import assert from 'assert';

const [, , infile] = process.argv;

(async () => {

    console.log("START")

    const backend = await import(`./build/index.main.mjs`);
    const stdlib = await loadStdlib();
    const startingBalance = stdlib.parseCurrency(100);

    const accAlice = await stdlib.newTestAccount(startingBalance);
    const accBob = await stdlib.newTestAccount(startingBalance);
    const accEve = await stdlib.newTestAccount(startingBalance);

    const zorkmid = await launchToken(stdlib, accAlice, "zorkmid", "ZMD");

    const getBalance = async (who) =>
        stdlib.formatCurrency(await stdlib.balanceOf(who), 4);

    const getMinBalance = async (who) =>
        stdlib.formatCurrency(await stdlib.minBalance(who))

    const beforeAlice = await getBalance(accAlice);
    const beforeBob = await getBalance(accBob);
    const beforeEve = await getBalance(accEve);

    const getParams = (addr) => ({
        addr,
        addr2: addr,
        addr3: addr,
        addr4: addr,
        addr5: addr,
        amt: stdlib.parseCurrency(1),
        tok: zorkmid.id,
        token_name: "",
        token_symbol: "",
        secs: 0,
        secs2: 0,
    });

    // (1) alice and bob tie vote
    console.log("ALICE AND BOB WAIT IN LINE TO VOTE");
    await (async (acc, acc2) => {
        let addr = acc.networkAccount.addr
        let ctc = acc.contract(backend)
        Promise.all([
            backend.Constructor(ctc, {
                getParams: () => getParams(addr),
                signal: () => { }
            }),
        ])
        let appId = await ctc.getInfo();
        console.log(appId)
        let ctc2 = acc2.contract(backend, appId);
        Promise.all([
            backend.Contractee(ctc2, {})
        ])
        await stdlib.wait(20)
    })(accAlice, accBob)

    const afterAlice = await getBalance(accAlice);
    const afterBob = await getBalance(accBob);
    const afterEve = await getBalance(accEve);

    const diffAlice = Math.round(afterAlice - beforeAlice)
    const diffBob = Math.round(afterBob - beforeBob)
    const diffEve = Math.round(afterEve - beforeEve)

    console.log(`Alice went from ${beforeAlice} to ${afterAlice} (${diffAlice}).`);
    console.log(`Bob went from ${beforeBob} to ${afterBob} (${diffBob}).`);

    const minBalanceAlice = await getMinBalance(accAlice)
    const minBalanceBob = await getMinBalance(accBob)
    const minBalanceEve = await getMinBalance(accEve)
    console.log({
        minBalanceAlice,
        minBalanceBob,
        minBalanceEve
    })

    assert.equal(diffAlice, 1)
    assert.equal(diffBob, -1)
    assert.equal(minBalanceAlice, '0.55')
    assert.equal(minBalanceBob, '0.25')
    assert.equal(minBalanceEve, '0.1')

    process.exit()

})();