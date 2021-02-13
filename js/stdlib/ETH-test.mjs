import { runTests, describe, it, expect } from './tester.mjs';
import * as stdlib from './ETH.mjs';
import * as ALGO_stdlib from './ALGO.mjs';
import * as FAKE_stdlib from './FAKE.mjs';

runTests(async () => {
  await describe('The `web3` stdlib', async () => {
    const { bigNumberify, ge, le, getNetworkTime, add } = stdlib;

    describe('exposes a `bigNumberToHex` function that', () => {
      it('correctly translates positive `BigNumber`s to hex', () => {
        const { bigNumberToHex } = stdlib;

        expect(bigNumberToHex(0)).toBe('0000000000000000000000000000000000000000000000000000000000000000');
        expect(bigNumberToHex(1)).toBe('0000000000000000000000000000000000000000000000000000000000000001');
        expect(bigNumberToHex(10)).toBe('000000000000000000000000000000000000000000000000000000000000000a');
        expect(bigNumberToHex(25)).toBe('0000000000000000000000000000000000000000000000000000000000000019');
        expect(bigNumberToHex(30)).toBe('000000000000000000000000000000000000000000000000000000000000001e');

        expect(bigNumberToHex(5463728190))
          .toBe('0000000000000000000000000000000000000000000000000000000145a9e03e');
      });

      it('correctly translates negative `BigNumber`s to hex', () => {
        const { bigNumberToHex } = stdlib;

        expect(bigNumberToHex(-1)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff');
        expect(bigNumberToHex(-10)).toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6');
        expect(bigNumberToHex(-30)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2');

        expect(bigNumberToHex(-5463728190))
          .toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2');
      });
    });

    await describe('T_Address.canonicalize can handle multiple inputs', async () => {
      const acc = await stdlib.createAccount();
      const addr = acc.networkAccount.address;
      const { canonicalize } = stdlib.T_Address;
      it('T_Address.canonicalize(accRelay) returns address', () => {
        expect(canonicalize(acc)).toBe(addr);
      });
      it('T_Address.canonicalize(accRelay.networkAccount) returns address', () => {
        expect(canonicalize(acc.networkAccount)).toBe(addr);
      });
      it('T_Address.canonicalize(accRelay.networkAccount.address) returns address', () => {
        expect(canonicalize(acc.networkAccount.address)).toBe(addr);
      });
    });

    describe('exposes a `stringToHex` function that', () => {
      const hand = 'ROCK';
      const handHex = stdlib.stringToHex(hand);

      it('works correctly with `bytes_eq`', () => {
        expect(stdlib.bytesEq(handHex, hand)).toBe(true);
      });
    });

    describe('exposes a `bigNumberify` function that', () => {
      it('correctly translates integer inputs to their `BigNumber` equivalents', () =>
        expect(bigNumberify(500).toString()).toBe('500'));

      it('correctly translates string inputs to their `BigNumber` equivalents', () =>
        expect(bigNumberify('1234567').toString()).toBe('1234567'));
    });


    describe('exposes an `isBigNumber` function that', () => {
      it('returns `true` for `BigNumber` arguments', () =>
        expect(stdlib.isBigNumber(bigNumberify('987654321'))).toBe(true));

      it('returns `false` for non-`BigNumber` arguments', () => {
        const { isBigNumber } = stdlib;

        expect(isBigNumber(98765.4321)).toBe(false);
        expect(isBigNumber('98765.43')).toBe(false);
        expect(isBigNumber({})).toBe(false);
      });
    });


    describe('exposes a `BigNumber` comparison function called', () => {

      describe('`eq` that', () => {
        it('returns `true` when its arguments match', () =>
          expect(stdlib.eq(bigNumberify(567890), bigNumberify(567890)))
          .toBe(true));

        it('returns `false` when provided mismatched arguments', () =>
          expect(stdlib.eq(bigNumberify(1), bigNumberify(2)))
          .toBe(false));
      });

      describe('`ge` that', () => {
        it('returns `true` when its first argument is greater than or equal to its second', () => {
          const { ge, bigNumberify } = stdlib;

          expect(ge(bigNumberify(5), bigNumberify(5))).toBe(true);
          expect(ge(bigNumberify(5), bigNumberify(4))).toBe(true);
        });

        it('returns `false` when its first argument is less than its second', () =>
          expect(stdlib.ge(bigNumberify(5), bigNumberify(6)))
          .toBe(false));
      });

      describe('`gt` that', () => {
        it('returns `true` when its first argument is greater than its second', () =>
          expect(stdlib.gt(bigNumberify(5), bigNumberify(4)))
          .toBe(true));

        it('returns `false` when its first argument is equal to or less than its second', () => {
          const { gt, bigNumberify } = stdlib;

          expect(gt(bigNumberify(5), bigNumberify(5))).toBe(false);
          expect(gt(bigNumberify(5), bigNumberify(6))).toBe(false);
        });
      });

      describe('`le` that', () => {
        it('returns `true` when its first argument is lesser than or equal to its second', () => {
          const { le, bigNumberify } = stdlib;

          expect(le(bigNumberify(5), bigNumberify(5))).toBe(true);
          expect(le(bigNumberify(4), bigNumberify(5))).toBe(true);
        });

        it('returns `false` when its first argument is greater than its second', () =>
          expect(stdlib.le(bigNumberify(6), bigNumberify(5)))
          .toBe(false));
      });

      describe('`lt` that', () => {
        it('returns `true` when its first argument is lesser than its second', () =>
          expect(stdlib.lt(bigNumberify(4), bigNumberify(5)))
          .toBe(true));

        it('returns `false` when its first argument is equal to or greater than its second', () => {
          const { lt, bigNumberify } = stdlib;

          expect(lt(bigNumberify(5), bigNumberify(5))).toBe(false);
          expect(lt(bigNumberify(6), bigNumberify(5))).toBe(false);
        });
      });
    });


    describe('exposes a `BigNumber` arithmetic function called', () => {
      it('`add` that sums its arguments', () =>
        expect(stdlib.add(bigNumberify(12), bigNumberify(1))
          .eq(bigNumberify(13)))
        .toBe(true));

      it('`sub` that subtracts its second argument from its first', () =>
        expect(stdlib.sub(bigNumberify(12), bigNumberify(1))
          .eq(bigNumberify(11)))
        .toBe(true));

      it('`mod` that returns the remainder of its first argument divided by its second', () =>
        expect(stdlib.mod(bigNumberify(10), bigNumberify(4))
          .eq(bigNumberify(2)))
        .toBe(true));

      it('`mul` that returns the product of its arguments', () =>
        expect(stdlib.mul(bigNumberify(3), bigNumberify(5))
          .eq(bigNumberify(15)))
        .toBe(true));
    });


    await describe('exposes a `newTestAccount` function which', async () => {
      const balance = Math.floor(Math.random() * 1000);

      await describe('accepts numeric arguments of type', async () => {
        await it('`BigNumber`', async () => {
          const a = await stdlib.newTestAccount(bigNumberify(balance));
          const b = await stdlib.balanceOf(a);

          expect(stdlib.eq(balance, b))
            .toBe(true);
        });

        await it('raw JavaScript `Number`', async () => {
          const a = await stdlib.newTestAccount(balance);
          const b = await stdlib.balanceOf(a);

          expect(stdlib.eq(balance, b))
            .toBe(true);
        });
      });
    });


    describe('protect', () => {
      const hello = 'hello';
      const helloHex = stdlib.stringToHex('hello');
      const addr = '0xdeadbeef';
      const n = 10;
      const bn = bigNumberify(n);

      const {
        protect,
        T_Null,
        T_Bool,
        T_Bytes,
        T_Address,
        T_UInt,
        T_Object,
        T_Array,
        T_Tuple,
        T_Data,
      } = stdlib;

      const T_MaybeInt = T_Data({ 'Some': T_UInt, 'None': T_Null });
      it('converts nully things to Null', () => {
        expect(protect(T_Null, null)).toBe(null);
        expect(protect(T_Null, undefined)).toBe(null);
      });
      it('converts bytesy things to Bytes', () => {
        expect(protect(T_Bytes(5), hello)).toBe(hello);
        expect(protect(T_Bytes(5), helloHex)).toBe(helloHex);
      });
      it('converts numbery things to UInt', () => {
        expect(protect(T_UInt, n)).toBe(bn);
        expect(protect(T_UInt, bn)).toBe(bn);
      });
      it('handles Bool', () => {
        expect(protect(T_Bool, true)).toBe(true);
      });
      it('handles Address', () => {
        expect(protect(T_Address, addr)).toBe(addr);
      });
      it('recurses into Tuples', () => {
        expect(protect(T_Tuple([T_UInt]), [n])).toBe([bn]);
      });
      it('recurses into Objects', () => {
        expect(protect(T_Object({ 'x': T_Bytes(5) }), { 'x': hello })).toBe({ 'x': hello });
      });
      it('recurses into Arrays', () => {
        expect(protect(T_Array(T_Null, 1), [undefined])).toBe([null]);
      });
      it('recurses into Data', () => {
        expect(protect(T_MaybeInt, ['Some', n])).toBe(['Some', bn]);
        expect(protect(T_MaybeInt, ['None', undefined])).toBe(['None', null]);
      });
    });

    describe('exports', () => {
      const stdlibExports = Object.keys(stdlib).sort();

      const ETH_extra_exports = [
        'setProvider',
      ];
      const ALGO_extra_exports = [
        'newAccountFromAlgoSigner',
        'setAlgodClient',
        'setBrowser',
        'setIndexer',
        'setWaitPort',
      ];
      const FAKE_extra_exports = [];

      for (const [otherName, otherStdlib, otherExtraExports] of [
          ['ALGO', ALGO_stdlib, ALGO_extra_exports],
          ['FAKE', FAKE_stdlib, FAKE_extra_exports],
        ]) {
        const otherStdlibExports = Object.keys(otherStdlib).sort();
        it(`should only export a few extra things compared to ${otherName}`, () =>
          expect(stdlibExports.filter(x => !otherStdlibExports.includes(x)))
          .toBe(ETH_extra_exports));

        it(`should export everything that ${otherName} does`, () =>
          expect(otherStdlibExports.filter(x => !stdlibExports.includes(x)))
          .toBe(otherExtraExports));
      }
    });

    describe('currency conversions', () => {
      const amt = '123.456';
      // 3 is the number of decimals to shift over to make 123.456 a whole number
      const amtWei = bigNumberify('10').pow(18 - 3).mul('123456');
      const amtTruncTo2 = amt.slice(0, amt.length - 1); // truncates! does not round
      it(`should convert to WEI correctly`, () => {
        expect(stdlib.parseCurrency(amt)).toBe(amtWei);
      });
      it(`should convert from WEI correctly`, () => {
        expect(stdlib.formatCurrency(amtWei)).toBe(amt);
        expect(stdlib.formatCurrency(amtWei, 2)).toBe(amtTruncTo2);
      });

      // Ok I know this is ETH-test, but... I'm lazy ~ Dan
      // 3 is the number of decimals to shift over to make 123.456 a whole number
      const amtMicroAlgos = bigNumberify('10').pow(6 - 3).mul('123456');
      it(`should convert to microAlgos correctly`, () => {
        expect(ALGO_stdlib.parseCurrency(amt)).toBe(amtMicroAlgos);
      });
      it(`should convert from microAlgos correctly`, () => {
        expect(ALGO_stdlib.formatCurrency(amtMicroAlgos)).toBe(amt);
        expect(ALGO_stdlib.formatCurrency(amtMicroAlgos, 2)).toBe(amtTruncTo2);
      });

      // Excess is truncated; no such thing as fractional reachies.
      const amtReachiesStr = '123';
      const amtReachies = bigNumberify(amtReachiesStr);
      it(`should convert to reachies correctly`, () => {
        expect(FAKE_stdlib.parseCurrency(amtReachiesStr)).toBe(amtReachies);
      });
      it(`should convert from reachies correctly`, () => {
        expect(FAKE_stdlib.formatCurrency(amtReachies)).toBe(amtReachiesStr);
        // Again, no decimal places for reachies.
        expect(FAKE_stdlib.formatCurrency(amtReachies, 0)).toBe(amtReachiesStr);
      });

    });

    await describe('wait', async () => {
      // Note: this test could go faster with a faster pollingInterval
      // I think.
      const begin = await getNetworkTime();

      let prog0 = 1;
      const first = await stdlib.wait(1, ({ currentTime }) => {
        describe(`prog0: ${prog0}`, () => {
          expect(ge(currentTime, 0) && le(currentTime, add(begin, prog0)))
            .toBe(true);
          prog0++;
        });
      });
      expect(ge(first, add(begin, 1)))
        .toBe(true);

      let prog1 = 1;
      const second = await stdlib.wait(1, ({ currentTime }) => {
        describe(`prog1: ${prog1}`, () => {
          expect(ge(currentTime, 1) && le(currentTime, add(begin, prog1 + 1)))
            .toBe(true);
          prog1++;
        });
      });
      expect(ge(second, add(begin, 2)))
        .toBe(true);

      let prog2 = 1;
      const third = await stdlib.waitUntilTime(add(second, 5), ({ currentTime }) => {
        describe(`prog2: ${prog2}`, () => {
          expect(ge(currentTime, 2) && le(currentTime, add(begin, prog2 + 2)))
            .toBe(true);
          prog2++;
        });
      });
      expect(ge(third, add(second, 5)))
        .toBe(true);
    });

  });
});
