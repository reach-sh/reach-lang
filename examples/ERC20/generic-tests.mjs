import * as backend from './build/index.main.mjs';
import * as backendTransferTest from './build/index.transferTest.mjs';
import { loadStdlib, ask } from "@reach-sh/stdlib";
const stdlib = loadStdlib(process.env);

const bigNumberify = stdlib.bigNumberify;
const assert = stdlib.assert;
const assertFail = async (promise) => {
  try {
    await promise;
  } catch (e) {
    return;
  }
  throw "Expected exception but did not catch one";
}
const assertEq = (a, b, context = "assertEq") => {
  if (a === b) return;
  try {
    const res1BN = bigNumberify(a);
    const res2BN = bigNumberify(b);
    if (res1BN.eq(res2BN)) return;
  } catch {}
  assert(false, `${context}: ${a} == ${b}`);
}

const startMeUp = async (ctc, meta) => {
  const flag = "startup success throw flag"
  try {
    await ctc.p.Deployer({
      meta,
      deployed: (ctc) => {
        throw flag;
      },
    });
  } catch (e) {
    if ( e !== flag) {
      throw e;
    }
  }
}

export const genericTests = async () => {
  const zeroAddress = "0x" + "0".repeat(40);
  const accs = await stdlib.newTestAccounts(4, stdlib.parseCurrency(100));
  accs.forEach((acc) => acc.setGasLimit(5000000));
  const [acc0, acc1, acc2, acc3] = accs;
  const [addr0, addr1, addr2, addr3] = accs.map(a => a.getAddress());

  const totalSupply = 1000_00;
  const decimals = 2;

  const meta = {
    name: "Coinzz",
    symbol: "CZZ",
    decimals,
    totalSupply,
    zeroAddress,
  }

  const ctc0 = acc0.contract(backend);
  await startMeUp(ctc0, meta);
  const ctcinfo = await ctc0.getInfo();
  const ctc = (acc) => acc.contract(backend, ctcinfo);

  const assertBalances = async (bal0, bal1, bal2, bal3) => {
    assertEq(bal0, (await ctc0.v.balanceOf(acc0.getAddress()))[1]);
    assertEq(bal1, (await ctc0.v.balanceOf(acc1.getAddress()))[1]);
    assertEq(bal2, (await ctc0.v.balanceOf(acc2.getAddress()))[1]);
    assertEq(bal3, (await ctc0.v.balanceOf(acc3.getAddress()))[1]);
  }
  const assertEvent = async (event, ...expectedArgs) => {
    const e = await ctc0.events[event].next();
    const actualArgs = e.what;
    expectedArgs.forEach((expectedArg, i) => assertEq(actualArgs[i], expectedArg, `${event} field ${i}`));
  }

  const transfer = async (fromAcc, toAcc, amt) => {
    await ctc(fromAcc).a.transfer(toAcc.getAddress(), amt);
    await assertEvent("Transfer", fromAcc.getAddress(), toAcc.getAddress(), amt);
  }
  const transferFrom = async (spenderAcc, fromAcc, toAcc, amt, allowanceLeft) => {
    await ctc(spenderAcc).a.transferFrom(fromAcc.getAddress(), toAcc.getAddress(), amt);
    await assertEvent("Transfer", fromAcc.getAddress(), toAcc.getAddress(), amt);
    await assertEvent("Approval", fromAcc.getAddress(), spenderAcc.getAddress(), allowanceLeft);
  }
  const approve = async (fromAcc, spenderAcc, amt) => {
    await ctc(fromAcc).a.approve(spenderAcc.getAddress(), amt);
    await assertEvent("Approval", fromAcc.getAddress(), spenderAcc.getAddress(), amt);
  }

  //// start actually testing
  console.log("starting connector-generic tests")

  // initial transfer event upon minting (when launching contract)
  await assertEvent("Transfer", zeroAddress, acc0.getAddress(), totalSupply);
  await assertBalances(totalSupply, 0, 0, 0);

  // transfer of more than you have should fail
  await assertFail(transfer(acc1, acc2, 10));
  await assertFail(transferFrom(acc1, acc2, acc3, 10, 0));
  // transfer of zero should work even if you don't have any, based on my reading of the spec
  await transfer(acc1, acc2, 0);
  // transferFrom of zero should work even the from doesn't have any and the transferer has an allowance of 0, based on my reading of the spec.
  await transferFrom(acc1, acc2, acc3, 0, 0);

  await transfer(acc0, acc1, 10);
  await assertBalances(totalSupply - 10, 10, 0, 0);
  assertEq((await ctc0.v.allowance(addr0, addr3))[1], 0);
  await approve(acc0, acc3, 20);
  assertEq((await ctc0.v.allowance(addr0, addr3))[1], 20);
  await assertBalances(totalSupply - 10, 10, 0, 0);
  // transferFrom of more than an allowance should fail
  await assertFail(transferFrom(acc3, acc0, acc2, 100, 20));
  await transferFrom(acc3, acc0, acc2, 10, 10);
  assertEq((await ctc0.v.allowance(addr0, addr3))[1], 10);
  await assertBalances(totalSupply - 20, 10, 10, 0);
  await transferFrom(acc3, acc0, acc3, 10, 0);
  assertEq((await ctc0.v.allowance(addr0, addr3))[1], 0);
  await assertBalances(totalSupply - 30, 10, 10, 10);
  // transferFrom should use up the allowance
  await assertFail(transferFrom(acc3, acc0, acc3, 1, 0));

  // Even if you're rich, you can't transfer more than your balance.
  await assertFail(transfer(acc0, acc2, totalSupply - 10));

  await approve(acc0, acc1, 100);

  assertEq((await ctc0.v.name())[1], meta.name, "name()");
  assertEq((await ctc0.v.symbol())[1], meta.symbol, "symbol()");
  assertEq((await ctc0.v.totalSupply())[1], meta.totalSupply, "totalSupply()");
  assertEq((await ctc0.v.decimals())[1], meta.decimals, "decimals()");

  console.log("finished connector-generic tests")

  if (stdlib.connector === "ETH") {
    console.log("Running extra tests on ETH, where Tokens are ERC-20s.")

    // Our frontend stdlib.transfer should accept it.
    await stdlib.transfer(acc0, acc2, 10, ctcinfo);
    await assertBalances(totalSupply - 40, 10, 20, 10);
    await stdlib.transfer(acc0, acc1, 10, ctcinfo);
    await assertBalances(totalSupply - 50, 20, 20, 10);

    // And contracts should accept our ERC-20 as Token values.
    const helperCtcA = acc2.contract(backendTransferTest);
    const helperCtcB = acc3.contract(backendTransferTest, helperCtcA.getInfo());
    await Promise.all([
      helperCtcA.p.A({
        tok: ctcinfo,
        getAmount: () => 10,
      }),
      helperCtcB.p.B({}),
    ]);

    await assertBalances(totalSupply - 50, 20, 10, 20);

    console.log("Done with extra ETH tests")
  }
}
