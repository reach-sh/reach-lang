import { describe, it, expect } from '../tester.mjs';


export const mkStdlibNetworkCommon = async lib => {
  const {
    T_Address,
    T_Array,
    T_Bool,
    T_Bytes,
    T_Data,
    T_Null,
    T_Object,
    T_Tuple,
    T_UInt,

    createAccount,
    newTestAccount,

    balanceOf,
    getNetworkTime,
    protect,
    stringToHex,
    wait,
    waitUntilTime,

    bigNumberToHex,
    bigNumberToNumber,
    bigNumberify,
    isBigNumber,

    bytesEq,
    eq,
    ge,
    gt,
    le,
    lt,

    add,
    mod,
    mul,
    sub,
  } = lib;


  describe('exposes a `bigNumberToHex` function that', () => {
    it('correctly translates positive `BigNumber`s to hex', () => {
      expect(bigNumberToHex( 0)).toBe('0000000000000000000000000000000000000000000000000000000000000000');
      expect(bigNumberToHex( 1)).toBe('0000000000000000000000000000000000000000000000000000000000000001');
      expect(bigNumberToHex(10)).toBe('000000000000000000000000000000000000000000000000000000000000000a');
      expect(bigNumberToHex(25)).toBe('0000000000000000000000000000000000000000000000000000000000000019');
      expect(bigNumberToHex(30)).toBe('000000000000000000000000000000000000000000000000000000000000001e');

      expect(bigNumberToHex(5463728190))
        .toBe('0000000000000000000000000000000000000000000000000000000145a9e03e');
    });

    it('correctly translates negative `BigNumber`s to hex', () => {
      expect(bigNumberToHex( -1)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff');
      expect(bigNumberToHex(-10)).toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff6');
      expect(bigNumberToHex(-30)).toBe('ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffe2');

      expect(bigNumberToHex(-5463728190))
        .toBe('fffffffffffffffffffffffffffffffffffffffffffffffffffffffeba561fc2');
    });
  });


  describe('exposes a `bigNumberToNumber` function that', () =>
    it('correctly translates from `bigNumber` to `Number`', () => {
      for (let i = 50; i > 0; i--) {
        const n = Math.floor(Math.random() * 1000);

        expect(bigNumberToNumber(bigNumberify(n)))
          .toBe(n);
      }
    }));


  describe('exposes a `stringToHex` function that', () => {
    const hand    = 'ROCK';
    const handHex = stringToHex(hand);

    it('works correctly with `bytes_eq`', () =>
      expect(bytesEq(handHex, hand)).toBe(true));
  });


  describe('exposes a `bigNumberify` function that', () => {
    it('correctly translates integer inputs to their `BigNumber` equivalents', () =>
      expect(bigNumberify(500).toString()).toBe('500'));

    it('correctly translates string inputs to their `BigNumber` equivalents', () =>
      expect(bigNumberify('1234567').toString()).toBe('1234567'));
  });


  describe('exposes an `isBigNumber` function that', () => {
    it('returns `true` for `BigNumber` arguments', () =>
      expect(isBigNumber(bigNumberify('987654321'))).toBe(true));

    it('returns `false` for non-`BigNumber` arguments', () => {
      expect(isBigNumber(98765.4321)).toBe(false);
      expect(isBigNumber('98765.43')).toBe(false);
      expect(isBigNumber({})).toBe(false);
    });
  });


  describe('exposes a `BigNumber` comparison function called', () => {

    describe('`eq` that', () => {
      it('returns `true` when its arguments match', () =>
        expect(eq(bigNumberify(567890), bigNumberify(567890)))
          .toBe(true));

      it('returns `false` when provided mismatched arguments', () =>
        expect(eq(bigNumberify(1), bigNumberify(2)))
          .toBe(false));
    });

    describe('`ge` that', () => {
      it('returns `true` when its first argument is greater than or equal to its second', () => {
        expect(ge(bigNumberify(5), bigNumberify(5))).toBe(true);
        expect(ge(bigNumberify(5), bigNumberify(4))).toBe(true);
      });

      it('returns `false` when its first argument is less than its second', () =>
        expect(ge(bigNumberify(5), bigNumberify(6)))
          .toBe(false));
    });

    describe('`gt` that', () => {
      it('returns `true` when its first argument is greater than its second', () =>
        expect(gt(bigNumberify(5), bigNumberify(4)))
          .toBe(true));

      it('returns `false` when its first argument is equal to or less than its second', () => {
        expect(gt(bigNumberify(5), bigNumberify(5))).toBe(false);
        expect(gt(bigNumberify(5), bigNumberify(6))).toBe(false);
      });
    });

    describe('`le` that', () => {
      it('returns `true` when its first argument is lesser than or equal to its second', () => {
        expect(le(bigNumberify(5), bigNumberify(5))).toBe(true);
        expect(le(bigNumberify(4), bigNumberify(5))).toBe(true);
      });

      it('returns `false` when its first argument is greater than its second', () =>
        expect(le(bigNumberify(6), bigNumberify(5)))
          .toBe(false));
    });

    describe('`lt` that', () => {
      it('returns `true` when its first argument is lesser than its second', () =>
        expect(lt(bigNumberify(4), bigNumberify(5)))
          .toBe(true));

      it('returns `false` when its first argument is equal to or greater than its second', () => {
        expect(lt(bigNumberify(5), bigNumberify(5))).toBe(false);
        expect(lt(bigNumberify(6), bigNumberify(5))).toBe(false);
      });
    });
  });


  describe('exposes a `BigNumber` arithmetic function called', () => {
    it('`add` that sums its arguments', () =>
      expect(add(bigNumberify(12), bigNumberify(1))
          .eq(bigNumberify(13)))
        .toBe(true));

    it('`sub` that subtracts its second argument from its first', () =>
      expect(sub(bigNumberify(12), bigNumberify(1)).eq(bigNumberify(11)))
        .toBe(true));

    it('`mod` that returns the remainder of its first argument divided by its second', () =>
      expect(mod(bigNumberify(10), bigNumberify(4)).eq(bigNumberify(2)))
        .toBe(true));

    it('`mul` that returns the product of its arguments', () =>
      expect(mul(bigNumberify(3), bigNumberify(5)).eq(bigNumberify(15)))
        .toBe(true));
  });


  await describe('exposes a `newTestAccount` function which', async () => {
    const balance = Math.floor(Math.random() * 10000000);

    await describe('accepts numeric arguments of type', async () => {
      await it('`BigNumber`', async () => {
        const a = await newTestAccount(bigNumberify(balance));
        const b = await balanceOf(a);

        expect(eq(balance, b))
          .toBe(true);
      });

      await it('raw JavaScript `Number`', async () => {
        const a = await newTestAccount(balance);
        const b = await balanceOf(a);

        expect(eq(balance, b))
          .toBe(true);
      });
    });
  });


  await describe('exposes a `createAccount` function which', async () => {
    await it('constructs new accounts with zero balances', async () => {
      const a = await createAccount();

      expect(eq(await balanceOf(a), 0))
        .toBe(true);
    });
  });


  describe('protect', () => {
    const hello      = 'hello';
    const helloHex   = stringToHex('hello');
    const addr       = '0xdeadbeef';
    const n          = 10;
    const bn         = bigNumberify(n);
    const T_MaybeInt = T_Data({ 'Some': T_UInt, 'None': T_Null });

    it('converts nully things to Null', () => {
      expect(protect(T_Null, null     )).toBe(null);
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
      expect(protect(T_MaybeInt, ['Some', n        ])).toBe(['Some', bn]);
      expect(protect(T_MaybeInt, ['None', undefined])).toBe(['None', null]);
    });
  });


  await describe('wait', async () => {
    // Note: this test could go faster with a faster pollingInterval
    // I think.
    const begin = await getNetworkTime();

    let prog0 = 1;
    const first = await wait(1, ({ currentTime }) => {
      describe(`prog0: ${prog0}`, () => {
        expect(ge(currentTime, 0) && le(currentTime, add(begin, prog0)))
          .toBe(true);
        prog0++;
      });
    });
    expect(ge(first, add(begin, 1)))
      .toBe(true);

    let prog1 = 1;
    const second = await wait(1, ({ currentTime }) => {
      describe(`prog1: ${prog1}`, () => {
        expect(ge(currentTime, 1) && le(currentTime, add(begin, prog1 + 1)))
          .toBe(true);
        prog1++;
      });
    });
    expect(ge(second, add(begin, 2)))
      .toBe(true);

    let prog2 = 1;
    const third = await waitUntilTime(add(second, 5), ({ currentTime }) => {
      describe(`prog2: ${prog2}`, () => {
        expect(ge(currentTime, 2) && le(currentTime, add(begin, prog2 + 2)))
          .toBe(true);
        prog2++;
      });
    });
    expect(ge(third, add(second, 5)))
      .toBe(true);
  });
};


export const mkT_AddressCanonicalize = (lib, a, accessor, fields, expected) =>
  it(`T_address.canonicalize(accRelay${fields}) returns address`, () =>
    expect(lib.T_Address.canonicalize(accessor(a)))
      .toBe(expected));


// TODO ALGO currently fails with:
//   `Error: Please use newAccountFromAlgoSigner instead`
// TODO FAKE currently fails with:
//    `Error: null`
export const mkGetDefaultAccount = async lib =>
  describe('exposes a `getDefaultAccount` function which', async () => {
    const { getDefaultAccount, newTestAccount, bigNumberify, balanceOf, transfer, eq } = lib;

    await it('represents a faucet when running against a devnet', async () => {
      const a = await getDefaultAccount();
      const b = await newTestAccount(1000);

      await transfer(a, b, bigNumberify(19000));

      expect(eq(await balanceOf(b), 20000))
        .toBe(true);
    });
  });


// TODO ALGO currently fails with:
//    `Error: invalid arrayify value`
// TODO FAKE currently fails with:
//    `TypeError: newAccountFromSecret is not a function`
export const mkNewAccountFromSecret = async (lib, pow, sec) =>
  describe('exposes a `newAccountFromSecret` function which', async () => {
    const { bigNumberify, newTestAccount, newAccountFromSecret, transfer, eq, balanceOf } = lib;

    await it('begins life with a zero balance', async () => {
      const f = bigNumberify(10).pow(pow);
      const a = await newTestAccount(f.mul(2));
      const b = await newAccountFromSecret(sec);

      await transfer(a, b, f);

      expect(eq(await balanceOf(b), f))
        .toBe(true);
    });
  });


// TODO currently no consumer knows how to use this
export const mkNewAccountFromMnemonic = async (lib, pow, mon) =>
  describe('exposes a `newAccountFromMnemonic` function which', async () => {
    const { bigNumberify, newTestAccount, newAccountFromMnemonic, transfer, eq, balanceOf } = lib;

    await it('begins life with a zero balance', async () => {
      const f = bigNumberify(10).pow(pow);
      const a = await newTestAccount(f.mul(2));
      const b = await newAccountFromMnemonic(mon);

      await transfer(a, b, f);

      expect(eq(await balanceOf(b), f))
        .toBe(true);
    });
  });


export const mkConnectAccount = async (lib, accessor) =>
  describe('exposes a `connectAccount` function which connects when given', async () => {
    const { connectAccount, newTestAccount, bigNumberify, eq, balanceOf } = lib;

    await it(String(accessor), async () => {
      const a = await newTestAccount(bigNumberify(10).pow(10));
      const b = await connectAccount(accessor(a));

      expect(eq(await balanceOf(a), await balanceOf(b)))
        .toBe(true);
    });
  });


export const mkFundFromFaucet = async lib =>
  describe('exposes a `fundFromFaucet` function which', async () => {
    const { fundFromFaucet, createAccount, balanceOf, bigNumberify, eq } = lib;

    await it('can fund testnet accounts', async () => {
      const a = await createAccount();
      await fundFromFaucet(a, bigNumberify(1000));

      expect(eq(await balanceOf(a), 1000))
        .toBe(true);
    });
  });
