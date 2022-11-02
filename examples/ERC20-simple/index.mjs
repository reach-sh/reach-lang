import * as backend from './build/index.main.mjs';
import { loadStdlib } from "@reach-sh/stdlib";
const stdlib = loadStdlib({REACH_NO_WARN: 'Y'});

if(stdlib.connector !== 'ETH'){
  console.log('Sorry, this program is only compiled on ETH for now');
  process.exit(0);
};
console.log("Starting up...");

const assert = stdlib.assert;
const bigNumberify = stdlib.bigNumberify;

const assertFail = async (promise) => {
  try {
    await promise;
  } catch (e) {
    return;
  }
  throw "Expected exception but did not catch one";
};

const assertEq = (a, b, context = "assertEq") => {
  if (a === b) return;
  try {
    const res1BN = bigNumberify(a);
    const res2BN = bigNumberify(b);
    if (res1BN.eq(res2BN)) return;
  } catch {}
  assert(false, `${context}: ${a} == ${b}`);
};

const startMeUp = async (ctc, meta) => {
  const flag = "startup success throw flag"
  try {
    await ctc.p.Deployer({
      meta,
      launched: (ctc) => {
        throw flag;
      },
    });
  } catch (e) {
    if ( e !== flag) {
      throw e;
    }
  }
};

const zeroAddress = "0x" + "0".repeat(40);
const accs = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));
const [acc0, acc1, acc2, acc3] = accs;
const [addr0, addr1, addr2, addr3] = accs.map(a => a.getAddress());

const totalSupply = 100_000;
const decimals = 2;
const meta = {
  name: "Coinzz",
  symbol: "CZZ",
  decimals,
  totalSupply,
  zeroAddress,
};

const ctc0 = acc0.contract(backend);
await startMeUp(ctc0, meta);
console.log('Completed startMeUp');

const ctcinfo = await ctc0.getInfo();
const ctc = (acc) => acc.contract(backend, ctcinfo);

const assertBalances = async (bal0, bal1, bal2, bal3) => {
  assertEq(bal0, (await ctc0.v.balanceOf(acc0.getAddress()))[1]);
  assertEq(bal1, (await ctc0.v.balanceOf(acc1.getAddress()))[1]);
  assertEq(bal2, (await ctc0.v.balanceOf(acc2.getAddress()))[1]);
  assertEq(bal3, (await ctc0.v.balanceOf(acc3.getAddress()))[1]);
  console.log('assertBalances complete');
};

const assertEvent = async (event, ...expectedArgs) => {
  const e = await ctc0.events[event].next();
  const actualArgs = e.what;
  expectedArgs.forEach((expectedArg, i) => assertEq(actualArgs[i], expectedArg, `${event} field ${i}`));
  console.log('assertEvent complete');
};

const transfer = async (fromAcc, toAcc, amt) => {
  await ctc(fromAcc).a.transfer(toAcc.getAddress(), amt);
  await assertEvent("Transfer", fromAcc.getAddress(), toAcc.getAddress(), amt);
  stdlib.transfer(fromAcc, toAcc, stdlib.parseCurrency(amt));
  console.log('transfer complete');
};

const transferFrom = async (spenderAcc, fromAcc, toAcc, amt, allowanceLeft) => {
  const b = await ctc(spenderAcc).a.transferFrom(fromAcc.getAddress(), toAcc.getAddress(), amt);
  await assertEvent("Transfer", fromAcc.getAddress(), toAcc.getAddress(), amt);
  await assertEvent("Approval", fromAcc.getAddress(), spenderAcc.getAddress(), allowanceLeft);
  console.log(`transferFrom complete is ${b}`);
};

const approve = async (fromAcc, spenderAcc, amt) => {
  await ctc(fromAcc).a.approve(spenderAcc.getAddress(), amt);
  await assertEvent("Approval", fromAcc.getAddress(), spenderAcc.getAddress(), amt);
  console.log('approve complete');
};


console.log("Starting tests...");

// initial transfer event upon minting (when launching contract)
await assertEvent("Transfer", zeroAddress, acc0.getAddress(), totalSupply);
console.log('assertEvent call complete');

// assert balances are equal to view values
// acc0 has the totalSupply at this point, all others are zero
await assertBalances(totalSupply, 0, 0, 0);
console.log('assertBalances call complete');

// transfer of more than you have should fail
await assertFail(transfer(acc1, acc2, 10));
await assertFail(transferFrom(acc1, acc2, acc3, 10, 0));
console.log('assertFail2 call complete');

// transfer of zero should work even if you don't have any
await transfer(acc1, acc2, 0);
console.log('transfer call complete');

// transferFrom of zero should work even the from doesn't have any and the transferer has an allowance of 0
await transferFrom(acc1, acc2, acc3, 0, 0);
console.log('transferFrom call complete');

// transfer 10 from acc0 to acc1
await transfer(acc0, acc1, 10);
// assert balances are correct after transfer
await assertBalances(totalSupply - 10, 10, 0, 0);

// assert the allowance for addr3 is 0
assertEq((await ctc0.v.allowance(addr0, addr3))[1], 0);
// approve the allowance for add3 to 20
await approve(acc0, acc3, 20);
// assert that allowance is correct
assertEq((await ctc0.v.allowance(addr0, addr3))[1], 20);
// check the balances again -- they haven't changed
await assertBalances(totalSupply - 10, 10, 0, 0);

// transferFrom of more than an allowance should fail
await assertFail(transferFrom(acc3, acc0, acc2, 100, 20));

// transferFrom spender, from, to, 10
await transferFrom(acc3, acc0, acc2, 10, 10);
// assert the allowance for addr3
assertEq((await ctc0.v.allowance(addr0, addr3))[1], 10);
// assertBalances updated after transfer
await assertBalances(totalSupply - 20, 10, 10, 0);
// transfer the 10 from acc3 back to acc0
await transferFrom(acc3, acc0, acc3, 10, 0);
// assert the allowance has changed
assertEq((await ctc0.v.allowance(addr0, addr3))[1], 0);
// check the balances
await assertBalances(totalSupply - 30, 10, 10, 10);
// transferFrom should use up the allowance
await assertFail(transferFrom(acc3, acc0, acc3, 1, 0));

// Even if you're rich, you can't transfer more than your balance.
await assertFail(transfer(acc0, acc2, totalSupply - 10));

// approve allowance of 100 tokens to acc0
await approve(acc0, acc1, 100);

// assert the view values are as expected
assertEq((await ctc0.v.name())[1], meta.name, "name()");
assertEq((await ctc0.v.symbol())[1], meta.symbol, "symbol()");
assertEq((await ctc0.v.totalSupply())[1], meta.totalSupply, "totalSupply()");
assertEq((await ctc0.v.decimals())[1], meta.decimals, "decimals()");

console.log("Finished testing!");
