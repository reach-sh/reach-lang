import { loadStdlib } from '@reach-sh/stdlib';
import * as backendVeriC from './build/index.veriC.mjs';
import * as backendTrapS from './build/index.trapS.mjs';
import * as backendTrapC from './build/index.trapC.mjs';
import * as backend1 from './build/index.main1.mjs';
import * as backend2 from './build/index.main2.mjs';
const stdlib = loadStdlib();
const bn = stdlib.bigNumberify;
const assertEq = (i, j, expected, actual) => {
  const exps = JSON.stringify(expected);
  const acts = JSON.stringify(actual);
  console.log('assertEq', {i,j,expected, actual}, {exps, acts});
  stdlib.assert(exps === acts); };
const mkA = async () => {
  const accA = await stdlib.newTestAccount(stdlib.parseCurrency(300));
  accA.setGasLimit(5000000);
  return accA;
};

const expectFail = async (name, f) => {
  try {
    await f();
    throw 42;
  } catch (e) {
    if ( e === 42 ) {
      throw Error(`${name} succeeded, but shouldn't`);
    }
    console.log(`Got error`, e);
  }
};

// VeriC
const veriC = async (x) => {
  const accA = await mkA();
  const ctcA = accA.contract(backendVeriC);
  await ctcA.p.A({ x });
}
await veriC(bn('777'));
if ( stdlib.connector === 'ALGO' ) {
  await expectFail(`veriC(big)`, () => veriC(bn(2).pow(128)));
}

// Test trap
const trapGo = async (f, ...args) => {
  const accA = await mkA();
  console.log(`Calling ${f}(${args})`);
  const ctcA_S = accA.contract(backendTrapS);
  const ctcA_C = accA.contract(backendTrapC);
  await ctcA_S.p.A({
    go: async (server) => {
      await ctcA_C.p.A({
        server,
        go: async () => {
          await ctcA_C.a[f](...args);
        }
      });
    },
  });
};
const trapNo = (f, ...args) =>
  expectFail(`${f}(${args})`, () => trapGo(f, ...args));

await trapGo('add', bn(1), bn(2));
await trapNo('add', bn(2).pow(256).sub(1), bn(2));
await trapGo('sub', bn(2), bn(1));
await trapNo('sub', bn(1), bn(2));
await trapGo('mul', bn(2), bn(4));
await trapNo('mul', bn(2).pow(129), bn(2).pow(129));
await trapGo('div', bn(4), bn(2));
await trapNo('div', bn(4), bn(0));
await trapGo('mod', bn(4), bn(2));
await trapNo('mod', bn(4), bn(0));
await trapGo('cast', bn(4));
await (( stdlib.connector === 'ALGO' ) ? trapNo : trapGo)('cast', bn(2).pow(80));

// Test main
const checkMain = async (which) => {
const accA = await mkA();
const ctcA = accA.contract(which ? backend1 : backend2);
const b1 = bn(2).pow(128);
const b2 = bn(2).pow(24);
const s1 = bn(2).pow(32);
const s2 = bn(2).pow(12);
const vs1 = [ b1, b2, s1, s2 ];
assertEq('vs1', 'vs1', [
  /*b1*/
  bn('0x0100000000000000000000000000000000'),
  /*b2*/
  bn('0x0000000000000000000000000001000000'),
  /*s1*/
  bn('0x0000000000000000000000000100000000'),
  /*s2*/
  bn('0x0000000000000000000000000000001000'),
], vs1);
const f = (a0, a1, a2, a3, a4, a5, a6) => [
  /* 0: b1+b2*/
  a0,
  /* 1: b1+b3*/
  a1,
  /* 2: b2+b3*/
  a2,
  /* 3: s1+s2*/
  a3,
  /* 4: s1+s3*/
  a4,
  /* 5: s1+b2s*/
  a5,
  /* 6: s2+s3*/
  a6,
  /* 7: s1b+s2b*/
  a3,
  /* 8: s1+s3*/
  a4,
  /* 9: s1+s2*/
  a3,
  /* A: s1+s3*/
  a4,
];
const answers = [
  // 0 - add
  f(
    /* 0: b1+b2*/
    bn('0x0100000000000000000000000001000000'),
    /* 1: b1+b3*/
    bn('0x0100000000000000000000000000000001'),
    /* 2: b2+b3*/
    bn('0x0000000000000000000000000001000001'),
    /* 3: s1+s2*/
    bn('0x0000000000000000000000000100001000'),
    /* 4: s1+s3*/
    bn('0x0000000000000000000000000100000001'),
    /* 5: s1+b2s*/
    bn('0x0000000000000000000000000101000000'),
    /* 6: s2+s3*/
    bn('0x0000000000000000000000000000001001'),
  ),
  // 1 - sub
  f(
    /* 0: b1-b2*/
    bn('0x00ffffffffffffffffffffffffff000000'),
    /* 1: b1-b3*/
    bn('0x00ffffffffffffffffffffffffffffffff'),
    /* 2: b2-b3*/
    bn('0x0000000000000000000000000000ffffff'),
    /* 3: s1-s2*/
    bn('0x00000000000000000000000000fffff000'),
    /* 4: s1-s3*/
    bn('0x00000000000000000000000000ffffffff'),
    /* 5: s1-b2s*/
    bn('0x00000000000000000000000000ff000000'),
    /* 6: s2-s3*/
    bn('0x0000000000000000000000000000000fff'),
  ),
  // 2 - mul
  f(
    /* 0: b1*b2*/
    bn('0x0100000000000000000000000000000000000000'),
    /* 1: b1*b3*/
    bn('0x0100000000000000000000000000000000'),
    /* 2: b2*b3*/
    bn('0x0000000000000000000000000001000000'),
    /* 3: s1*s2*/
    bn('0x0000000000000000000000000100000000000'),
    /* 4: s1*s3*/
    bn('0x0000000000000000000000000100000000'),
    /* 5: s1*b2s*/
    bn('0x0000000000000000000000000100000000000000'),
    /* 6: s2*s3*/
    bn('0x0000000000000000000000000000001000'),
  ),
  // 3 - div
  f(
    /* 0: b1/b2*/
    bn('0x0100000000000000000000000000'),
    /* 1: b1/b3*/
    bn('0x0100000000000000000000000000000000'),
    /* 2: b2/b3*/
    bn('0x0000000000000000000000000001000000'),
    /* 3: s1/s2*/
    bn('0x0000000000000000000000000100000'),
    /* 4: s1/s3*/
    bn('0x0000000000000000000000000100000000'),
    /* 5: s1/b2s*/
    bn('0x0000000000000000000000000100'),
    /* 6: s2/s3*/
    bn('0x0000000000000000000000000000001000'),
  ),
  // 4 - mod
  f(
    /* 0: b1%b2*/
    bn('0'),
    /* 1: b1%b3*/
    bn('0'),
    /* 2: b2%b3*/
    bn('0'),
    /* 3: s1%s2*/
    bn('0'),
    /* 4: s1%s3*/
    bn('0'),
    /* 5: s1%b2s*/
    bn('0'),
    /* 6: s2%s3*/
    bn('0'),
  ),
  // 5 - xor (same as add)
  f(
    /* 0: b1+b2*/
    bn('0x0100000000000000000000000001000000'),
    /* 1: b1+b3*/
    bn('0x0100000000000000000000000000000001'),
    /* 2: b2+b3*/
    bn('0x0000000000000000000000000001000001'),
    /* 3: s1+s2*/
    bn('0x0000000000000000000000000100001000'),
    /* 4: s1+s3*/
    bn('0x0000000000000000000000000100000001'),
    /* 5: s1+b2s*/
    bn('0x0000000000000000000000000101000000'),
    /* 6: s2+s3*/
    bn('0x0000000000000000000000000000001001'),
  ),
  // 6 - ior (same as add)
  f(
    /* 0: b1+b2*/
    bn('0x0100000000000000000000000001000000'),
    /* 1: b1+b3*/
    bn('0x0100000000000000000000000000000001'),
    /* 2: b2+b3*/
    bn('0x0000000000000000000000000001000001'),
    /* 3: s1+s2*/
    bn('0x0000000000000000000000000100001000'),
    /* 4: s1+s3*/
    bn('0x0000000000000000000000000100000001'),
    /* 5: s1+b2s*/
    bn('0x0000000000000000000000000101000000'),
    /* 6: s2+s3*/
    bn('0x0000000000000000000000000000001001'),
  ),
  // 7 - and (same as mod)
  f(
    /* 0: b1%b2*/
    bn('0'),
    /* 1: b1%b3*/
    bn('0'),
    /* 2: b2%b3*/
    bn('0'),
    /* 3: s1%s2*/
    bn('0'),
    /* 4: s1%s3*/
    bn('0'),
    /* 5: s1%b2s*/
    bn('0'),
    /* 6: s2%s3*/
    bn('0'),
  ),
  // 8 - lt
  f(
    /* 0: b1+b2*/
    false,
    /* 1: b1+b3*/
    false,
    /* 2: b2+b3*/
    false,
    /* 3: s1+s2*/
    false,
    /* 4: s1+s3*/
    false,
    /* 5: s1+b2s*/
    false,
    /* 6: s2+s3*/
    false,
  ),
  // 9 - le
  f(
    /* 0: b1+b2*/
    false,
    /* 1: b1+b3*/
    false,
    /* 2: b2+b3*/
    false,
    /* 3: s1+s2*/
    false,
    /* 4: s1+s3*/
    false,
    /* 5: s1+b2s*/
    false,
    /* 6: s2+s3*/
    false,
  ),
  // 10 - eq
  f(
    /* 0: b1+b2*/
    false,
    /* 1: b1+b3*/
    false,
    /* 2: b2+b3*/
    false,
    /* 3: s1+s2*/
    false,
    /* 4: s1+s3*/
    false,
    /* 5: s1+b2s*/
    false,
    /* 6: s2+s3*/
    false,
  ),
  // 11 - ge
  f(
    /* 0: b1+b2*/
    true,
    /* 1: b1+b3*/
    true,
    /* 2: b2+b3*/
    true,
    /* 3: s1+s2*/
    true,
    /* 4: s1+s3*/
    true,
    /* 5: s1+b2s*/
    true,
    /* 6: s2+s3*/
    true,
  ),
  // 12 - gt
  f(
    /* 0: b1+b2*/
    true,
    /* 1: b1+b3*/
    true,
    /* 2: b2+b3*/
    true,
    /* 3: s1+s2*/
    true,
    /* 4: s1+s3*/
    true,
    /* 5: s1+b2s*/
    true,
    /* 6: s2+s3*/
    true,
  ),
];
await ctcA.p.A({
  vs1,
  check: (ibn, rsh) => {
    const i = ibn.toNumber();
    rsh.forEach((v, j) =>
      assertEq(i, j, (answers[i]||[])[j], v))
  },
});
}

await checkMain(true);
await checkMain(false);
