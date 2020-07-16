import { runTests, describe, it, expect } from './tester.mjs';
import * as stdlib from './ETH.mjs';
import * as ALGO_stdlib from './ALGO.mjs';

runTests(() => { describe('The `web3` stdlib', () => {
  const toBN = stdlib.toBN;

  describe('exposes a `bnToHex` function that', () => {
    it('correctly translates positive `BigNumber`s to hex', () => {
      const { bnToHex } = stdlib;

      expect(bnToHex(0 )).toBe('0000000000000000000000000000000000000000000000000000000000000000');
      expect(bnToHex(1 )).toBe('0000000000000000000000000000000000000000000000000000000000000001');
      expect(bnToHex(10)).toBe('000000000000000000000000000000000000000000000000000000000000000a');
      expect(bnToHex(25)).toBe('0000000000000000000000000000000000000000000000000000000000000019');
      expect(bnToHex(30)).toBe('000000000000000000000000000000000000000000000000000000000000001e');

      expect(bnToHex(5463728190))
        .toBe('0000000000000000000000000000000000000000000000000000000145a9e03e');
    });

    it('correctly translates negative `BigNumber`s to hex', () => {
      const { bnToHex } = stdlib;

      expect(bnToHex(-1 )).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff');
      expect(bnToHex(-10)).toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6');
      expect(bnToHex(-30)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2');

      expect(bnToHex(-5463728190))
        .toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2');
    });
  });


  describe('exposes a `toBN` function that', () => {
    it('correctly translates integer inputs to their `BigNumber` equivalents', () =>
       expect(toBN(500).toString()).toBe('500'));

    it('correctly translates string inputs to their `BigNumber` equivalents', () =>
       expect(toBN('1234567').toString()).toBe('1234567'));
  });


  describe('exposes an `isBN` function that', () => {
    it('returns `true` for `BigNumber` arguments', () =>
       expect(stdlib.isBN(toBN('987654321'))).toBe(true));

    it('returns `false` for non-`BigNumber` arguments', () => {
      const { isBN } = stdlib;

      expect(isBN(98765.4321)).toBe(false);
      expect(isBN('98765.43')).toBe(false);
      expect(isBN({        })).toBe(false);
    });
  });


  describe('exposes a `BigNumber` comparison function called', () => {

    describe('`eq` that', () => {
      it('returns `true` when its arguments match', () =>
         expect(stdlib.eq(toBN(567890), toBN(567890)))
         .toBe(true));

      it('returns `false` when provided mismatched arguments', () =>
         expect(stdlib.eq(toBN(1), toBN(2)))
         .toBe(false));
    });

    describe('`ge` that', () => {
      it('returns `true` when its first argument is greater than or equal to its second', () => {
        const { ge, toBN } = stdlib;

        expect(ge(toBN(5), toBN(5))).toBe(true);
        expect(ge(toBN(5), toBN(4))).toBe(true);
      });

      it('returns `false` when its first argument is less than its second', () =>
         expect(stdlib.ge(toBN(5), toBN(6)))
         .toBe(false));
    });

    describe('`gt` that', () => {
      it('returns `true` when its first argument is greater than its second', () =>
         expect(stdlib.gt(toBN(5), toBN(4)))
         .toBe(true));

      it('returns `false` when its first argument is equal to or less than its second', () => {
        const { gt, toBN } = stdlib;

        expect(gt(toBN(5), toBN(5))).toBe(false);
        expect(gt(toBN(5), toBN(6))).toBe(false);
      });
    });

    describe('`le` that', () => {
      it('returns `true` when its first argument is lesser than or equal to its second', () => {
        const { le, toBN } = stdlib;

        expect(le(toBN(5), toBN(5))).toBe(true);
        expect(le(toBN(4), toBN(5))).toBe(true);
      });

      it('returns `false` when its first argument is greater than its second', () =>
         expect(stdlib.le(toBN(6), toBN(5)))
         .toBe(false));
    });

    describe('`lt` that', () => {
      it('returns `true` when its first argument is lesser than its second', () =>
         expect(stdlib.lt(toBN(4), toBN(5)))
         .toBe(true));

      it('returns `false` when its first argument is equal to or greater than its second', () => {
        const { lt, toBN } = stdlib;

        expect(lt(toBN(5), toBN(5))).toBe(false);
        expect(lt(toBN(6), toBN(5))).toBe(false);
      });
    });
  });


  describe('exposes a `BigNumber` arithmetic function called', () => {
    it('`add` that sums its arguments', () =>
       expect(stdlib.add(toBN(12), toBN(1))
              .eq( toBN(13)))
       .toBe(true));

    it('`sub` that subtracts its second argument from its first', () =>
       expect(stdlib.sub(toBN(12), toBN(1))
              .eq( toBN(11)))
       .toBe(true));

    it('`mod` that returns the remainder of its first argument divided by its second', () =>
       expect(stdlib.mod(toBN(10), toBN(4))
              .eq( toBN(2)))
       .toBe(true));

    it('`mul` that returns the product of its arguments', () =>
       expect(stdlib.mul(toBN(3), toBN(5))
              .eq( toBN(15)))
       .toBe(true));
  });

  describe('exports', () => {
    const stdlibExports = Object.keys(stdlib).sort();
    const algoStdlibExports = Object.keys(ALGO_stdlib).sort();

    it('should only export a few extra things compared to ALGO', () =>
       expect(stdlibExports.filter(x => !algoStdlibExports.includes(x)))
       .toBe(['fromWei', 'toWei', 'toWeiBN']));

    it('should export everything that ALGO does', () =>
       expect(algoStdlibExports.filter(x => !stdlibExports.includes(x)))
       .toBe([]));
  });
}); });
