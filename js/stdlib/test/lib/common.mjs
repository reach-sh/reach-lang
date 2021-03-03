import { strict as assert } from 'assert';
import * as shared          from '@reach-sh/stdlib/shared.mjs';


const AsyncFunction = (async () => {}).constructor;


export const run = async specs => {
  const s = [ ...specs ];
  let   f;

  // eslint-disable-next-line no-cond-assign
  while (f = s.shift()) {
    if (f instanceof AsyncFunction) {
      await f().catch(e => {
        console.log(e);
        process.exit(1);
      });
    } else {
      f();
    }

    if (s.length > 0)
      console.log('\n ---\n');
  }
};


let indent     = 0;
let lastReport = '';


const report = l => {
  const m = l.replace(' ', '') !== ''
    ? `${' '.repeat(indent)}${l}`
    : '';

  if ((m === '' && lastReport !== '') || m !== '')
    console.log(m);

  lastReport = m;
};


export const describe = async (label, f) => {
  report(label);
  indent += 2;

  f instanceof AsyncFunction
    ? await f()
    : f();

  indent -= 2;
  report('');
};


export const it = async (label, f) => {
  report(label);
  await f();
};


const mkToRaise = async (x, e, f) => {
  try {
    await x();
    expect(1)
      .toBe(0);

  } catch (ex) {
    expect(f(ex.message))
      .toBe(e);
  }
};


export const expect = x => ({
  toBe:    v => assert.deepStrictEqual(x, v),
  toRaise: e => mkToRaise(x, e, a => a),

  toEq: v => assert.deepStrictEqual(
    shared.eq(x, v), true, `${String(x)} !== ${String(v)}`),

  toNotEq: v => assert.deepStrictEqual(
    shared.eq(x, v), false, `${String(x)} === ${String(v)}`),

  toRaiseStartingWith: e => mkToRaise(x, e, a =>
    (a.match(`^${e}`) || []).shift() || a.substr(0, e.length)),
});


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
    fundFromFaucet,
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

        expect(balance)
          .toEq(b);
      });

      await it('raw JavaScript `Number`', async () => {
        const a = await newTestAccount(balance);
        const b = await balanceOf(a);

        expect(balance)
          .toEq(b);
      });
    });
  });


  await describe('exposes a `createAccount` function which', async () => {
    await it('constructs new accounts with zero balances', async () => {
      const a = await createAccount();

      expect(await balanceOf(a))
        .toEq(0);
    });
  });


  await describe('exposes a `fundFromFaucet` function which', async () => {
    await it('can fund testnet accounts with `BigNumber` values', async () => {
      const a = await createAccount();
      await fundFromFaucet(a, bigNumberify(100000));

      expect(await balanceOf(a))
        .toEq(100000);
    });

    await it('can fund testnet accounts with JavaScript `Number` values', async () => {
      const a = await createAccount();
      await fundFromFaucet(a, 100000);

      expect(await balanceOf(a))
        .toEq(100000);
    });
  });


  describe('exposes a `protect` function that', () => {
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
      expect(protect(T_MaybeInt, ['Some', n        ])).toBe(['Some', bn  ]);
      expect(protect(T_MaybeInt, ['None', undefined])).toBe(['None', null]);
    });
  });


  await describe('wait', async () => {
    // Note: this test could go faster with a faster pollingInterval
    // I think.
    const begin = await getNetworkTime();

    let prog0 = 1;
    const first = await wait(1, ({ currentTime }) => {
      it(`prog0: ${prog0}`, () => {
        expect(ge(currentTime, 0) && le(currentTime, add(begin, prog0)))
          .toBe(true);
        prog0++;
      });
    });
    expect(ge(first, add(begin, 1)))
      .toBe(true);

    let prog1 = 1;
    const second = await wait(1, ({ currentTime }) => {
      it(`prog1: ${prog1}`, () => {
        expect(ge(currentTime, 1) && le(currentTime, add(begin, prog1 + 1)))
          .toBe(true);
        prog1++;
      });
    });
    expect(ge(second, add(begin, 2)))
      .toBe(true);

    let prog2 = 1;
    const third = await waitUntilTime(add(second, 5), ({ currentTime }) => {
      it(`prog2: ${prog2}`, () => {
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
    const { getDefaultAccount, newTestAccount, bigNumberify, balanceOf, transfer } = lib;

    await it('represents a faucet when running against a devnet', async () => {
      const a = await getDefaultAccount();
      const b = await newTestAccount(1000);

      await transfer(a, b, bigNumberify(19000));

      expect(await balanceOf(b))
        .toEq(20000);
    });
  });


// TODO ALGO currently fails with:
//    `Error: invalid arrayify value`
// TODO FAKE currently fails with:
//    `TypeError: newAccountFromSecret is not a function`
export const mkNewAccountFromSecret = async (lib, pow, sec) =>
  describe('exposes a `newAccountFromSecret` function which', async () => {
    const { bigNumberify, newTestAccount, newAccountFromSecret, transfer, balanceOf } = lib;

    await it('begins life with a zero balance', async () => {
      const f = bigNumberify(10).pow(pow);
      const a = await newTestAccount(f.mul(2));
      const b = await newAccountFromSecret(sec);

      await transfer(a, b, f);

      expect(await balanceOf(b))
        .toEq(f);
    });
  });


// TODO currently no consumer knows how to use this
export const mkNewAccountFromMnemonic = async (lib, pow, mon) =>
  describe('exposes a `newAccountFromMnemonic` function which', async () => {
    const { bigNumberify, newTestAccount, newAccountFromMnemonic, transfer, balanceOf } = lib;

    await it('begins life with a zero balance', async () => {
      const f = bigNumberify(10).pow(pow);
      const a = await newTestAccount(f.mul(2));
      const b = await newAccountFromMnemonic(mon);

      await transfer(a, b, f);

      expect(await balanceOf(b))
        .toEq(f);
    });
  });


export const mkConnectAccount = async (lib, accessor) =>
  describe('exposes a `connectAccount` function which connects when given', async () => {
    const { connectAccount, newTestAccount, bigNumberify, balanceOf } = lib;

    await it(String(accessor), async () => {
      const a = await newTestAccount(bigNumberify(10).pow(10));
      const b = await connectAccount(accessor(a));

      expect(await balanceOf(a))
       .toEq(await balanceOf(b));
    });
  });


export const mkKont = async lib =>
  describe('exposes a `mkKont` constructor which provides', async () => {
    const { mkKont } = lib;

    await describe('an `id` function that', async () => {
      const K = mkKont();

      await it('raises an exception for untracked IDs', async () =>
        (await expect(() => K.id('no such ID'))
          .toRaise(K._.untracked('no such ID'))));

      await it('fetches the entry indexed by a given ID', async () => {
        const a = { a: { unique: { entry: true }}};
        const b = 'something else entirely';

        expect(K.id(await K.track(a)))
          .toBe(a);

        expect(K.id(await K.track(b)))
          .toBe(b);
      });
    });

    await describe('a `track` function that', async () => {
      await describe('generates IDs of the form `<nonce>_<random hex>`', async () => {
        const deconstructed = i => {
          const  [ pre, suf ] = i.split('_');
          return { pre, suf: suf || '' };
        };

        await it('which index its `a` argument in `k`', async () => {
          const K = mkKont();
          const a = { foo: 'bar' };
          const i = await K.track(a);

          expect(K.id(i))
            .toBe(a);

          expect(K._.k[i])
            .toBe(a);
        });

        await describe('where the nonce prefix', async () => {
          await it('always starts at 0', async () => {
            for (let l = 0; l < 100; l++) {
              const K = mkKont();
              const d = deconstructed(await K.track(null));

              expect(Number(d.pre))
                .toBe(0);
            }
          });

          await it('is incremented by 1 for each entry `track`ed', async () => {
            const K = mkKont();

            for (let l = 0; l < 100; l++) {
              const d = deconstructed(await K.track(null));

              expect(Number(d.pre))
                .toBe(l);
            }
          });
        });

        await describe('where the random hex suffix is', async () => {
          await it('of length 48', async () => {
            const K = mkKont();

            for (let i = 100; i > 0; i--) {
              const d = deconstructed(await K.track(null));

              expect(d.suf.length)
                .toBe(48);
            }
          });

          await it('guaranteed to be lowercase, non-0x hexadecimal', async () => {
            const K     = mkKont();
            const isHex = s => /^[0-9a-f]+$/.test(s);
            const a     = deconstructed(await K.track(null));

            expect(isHex(`0x${a.suf}`))
              .toBe(false);

            for (let i = 100; i > 0; i--) {
              const b = deconstructed(await K.track(null));

              expect(isHex(b.suf))
                .toBe(true);
            }
          });
        });
      });
    });

    await describe('a `replace` function that', async () => {
      await it('swaps the entry indexed by a given ID and returns the same ID', async () => {
        const K = mkKont();
        const a = 'first';
        const b = 'second';
        const i = await K.track(a);

        expect(K.id(i))
          .toBe(a);

        expect(K.replace(i, b))
          .toBe(i);

        expect(K.id(i))
          .toBe(b);
      });

      await it('raises an exception for untracked IDs', async () => {
        const K              = mkKont();
        const [ ai, bi, ci ] = [ await K.track(1), await K.track(1), await K.track(1) ];

        K.forget(ai);
        K.forget(ci);

        expect(() => K.replace(ai, 2))
          .toRaise(K._.untracked(ai));

        expect(K.replace(bi, 2))
          .toBe(bi);

        expect(() => K.replace(ci, 2))
          .toRaise(K._.untracked(ci));
      });
    });

    await describe('a `forget` function that', async () => {
      await it('clears a given index from `k`', async () => {
        const K = mkKont();

        const a = [ 'something', 'else' ];
        const b = { forget: 'me' };
        const c = { leave: { me: 'in memory please' }};

        const [ ai, bi, ci ] = [ await K.track(a), await K.track(b), await K.track(c) ];

        /* ~ */ K.forget(bi); /* ~ */

        await expect(() => K.id(bi))
          .toRaise(K._.untracked(bi));

        expect(K.id(ai))
          .toBe(a);

        expect(K._.k[bi])
          .toBe(undefined);

        expect(K.id(ci))
          .toBe(c);

        /* ~ */ K.forget(ai); /* ~ */

        await expect(() => K.id(ai))
          .toRaise(K._.untracked(ai));

        expect(K._.k[ai])
          .toBe(undefined);

        expect(K._.k[bi])
          .toBe(undefined);

        expect(K.id(ci))
          .toBe(c);
      });
    });

    await describe('a `was` namespace which exposes', async () => {
      await describe('an `untracked` function that can correctly', async () => {
        const K = mkKont();

        await it('identify exceptions raised due to untracked IDs', async () => {
          try {
            K.id('d3adb33f');
          } catch (e) {
            expect(K.was.untracked(e))
              .toBe(true);
          }
        });

        await it('distinguish other exceptions', async () => {
          try {
            throw new Error('nope');
          } catch (e) {
            expect(K.was.untracked(e))
              .toBe(false);
          }
        });
      });
    });
  });
