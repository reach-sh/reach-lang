'reach 0.1';

export function not (x) {
  return (x ? false : true); }
export const boolEq = (x, y) => (x ? y : !y);

export function polyNeq (x, y) {
  return not(x == y); }

// Operator aliases
export const add = (x, y) => x + y;
export const sub = (x, y) => x - y;
export const mul = (x, y) => x * y;
export const div = (x, y) => x / y;
export const mod = (x, y) => x % y;
export const lt  = (x, y) => x < y;
export const le  = (x, y) => x <= y;
export const eq  = (x, y) => x == y;
export const ge  = (x, y) => x >= y;
export const gt  = (x, y) => x > y;
export const ite = (b, x, y) => b ? x : y;
export const lsh = (x, y) => x << y;
export const rsh = (x, y) => x >> y;
export const band = (x, y) => x & y;
export const bior = (x, y) => x | y;
export const bxor = (x, y) => x ^ y;
export const or = (x, y) => x || y;
export const and = (x, y) => x && y;

// Library functions
export const Maybe = (A) => Data({None: Null, Some: A});
export const fromMaybe = (v, onNull, onSome) => {
  switch (v) {
  case None: return onNull();
  case Some: return onSome(v); } };

export const isSome = m => m.match({
  Some: (_) => { return true; },
  None: (_) => { return false; } });

export const isNone = m => m.match({
  Some: (_) => { return false; },
  None: (_) => { return true; } });

export function implies (x, y) {
  return (not(x) || y); }

export function ensure(f, x) {
  assert(f(x));
  return x; }

export const hasRandom = {
  random: Fun([], UInt) };

export function makeCommitment (interact, x) {
  const salt = interact.random();
  const commitment = digest(salt, x);
  return [commitment, salt]; }

export function checkCommitment (commitment, salt, x) {
  return require(commitment == digest(salt, x)); }

export function closeTo(Who, after) {
  Who.publish();
  transfer(balance()).to(Who);
  commit();
  after();
  exit(); }

export const fail = () => assume(false);

export const Either = (A, B) => Data({
  Left: A,
  Right: B });

export const either = (e, l, r) =>
  e.match({
    Left: lv => { return l(lv); },
    Right: rv => { return r(rv); }
  });

export const isLeft = e => e.match({
  Left: (_) => { return true; },
  Right: (_) => { return false; } });

export const isRight = e => e.match({
  Left: (_) => { return false; },
  Right: (_) => { return true; } });

export const fromLeft = (e, d) => e.match({
  Left: (v) => { return v; },
  Right: (_) => { return d; } });

export const fromRight = (e, d) => e.match({
  Left: (_) => { return d; },
  Right: (v) => { return v; } });

// Standard library functions that should be hidden in some way, like
// SLV_HaskellFunction FIXME

export const Object_set = (o, k, e) =>
  ({...o, [k]: e});
export const Object_setIfUnset = (o, k, dv) =>
  Object.has(o, k) ? o : Object.set(o, k, dv);
export const Array_empty =
  Array.iota(0);
export const Array_replicate =
  (sz, v) => Array.iota(sz).map(x => v);
export const Array_forEach =
  (arr, f) => arr.reduce(null, (acc, xe) => f(xe));
export const Array_forEach1 =
  (arr) => (f) => Array_forEach(arr, f);

export const Array_min =
  (arr) => arr.reduce(UInt.max, (acc, x) => (x < acc) ? x : acc);
export const Array_min1 = (arr) => () => Array_min(arr);

export const Array_max =
  (arr) => arr.reduce(0, (acc, x) => (x > acc) ? x : acc);
export const Array_max1 = (arr) => () => Array_max(arr);

export const Array_any =
  (arr, f) => arr.reduce(false, (acc, x) =>
    acc ? acc : f(x));
export const Array_any1 = (arr) => (f) => Array_any(arr, f);

export const Array_all =
  (arr, f) => arr.reduce(true, (acc, x) =>
    acc ? f(x) : false);
export const Array_all1 = (arr) => (f) => Array_all(arr, f);

export const Array_or =
  (arr) => Array_any(arr, x => x);
export const Array_or1 = (arr) => () => Array_or(arr);

export const Array_and =
  (arr) => Array_all(arr, x => x);
export const Array_and1 = (arr) => () => Array_and(arr);

export const Array_sum =
  (arr) => arr.reduce(0, (acc, x) => acc + x);
export const Array_sum1 = (arr) => () => Array_sum(arr);

export const Array_includes =
  (arr, x) => arr.reduce(false, (acc, e) =>
    acc ? acc : (x == e) ? true : false);
export const Array_includes1 =
  (arr) => (x) => Array_includes(arr, x);

export const Array_indexOfAux =
  (arr, f) => {
    const init = [Maybe(UInt).None(), 0];
    const [res, _] =
      arr.reduce(init, (acc, e) => {
        const [foundIdx, idx] = acc;
        return foundIdx.match({
          Some: (_) => { return acc; },
          None: () => {
            return f(e)
              ? [Maybe(UInt).Some(idx), idx]
              : [foundIdx, idx + 1]; }
        });
      });
    return res;
  };
export const Array_indexOf =
  (arr, x) => Array_indexOfAux(arr, (el) => { return el == x; });
export const Array_indexOf1 =
  (arr) => (x) => Array_indexOf(arr, x);

export const Array_findIndex =
  (arr, f) => Array_indexOfAux(arr, f);
export const Array_findIndex1 =
  (arr) => (f) => Array_findIndex(arr, f);

export const Array_count =
  (arr, f) => arr.reduce(0, (acc, x) =>
    f(x) ? acc + 1 : acc);
export const Array_count1 =
  (arr) => (f) => Array_count(arr, f);

export const compose =
  (f, g) => (v) => f(g(v));

export const Array_average = (arr) =>
  arr.sum() / arr.length;

export const Array_average1 = (arr) => () =>
  Array_average(arr);

export const Array_product = (arr) =>
  arr.reduce(1, (acc, x) => acc * x);

export const Array_product1 = (arr) => () =>
  Array_product(arr);

export const sqrt = (y, k) =>
  Array.iota(k).reduce([ y, (y / 2 + 1) ], ([ z, x ], a) =>
    (x < z)
      ? [ x, ((y / x + x) / 2) ]
      : [ z, x ]
      )[1];

export const Int = Object({ sign: Bool, i: UInt});
export const int = (sign, i) => ({ sign, i });

export const Pos = true;
export const Neg = false;

// Operator abbreviation expansions
export const minus = (x) => ({ i: x.i, sign: !x.sign });
export const plus = (x) => x;

export const igt = (x, y) => {
  const t = [ x.sign, y.sign ];
  return (
      (t == [ Pos, Neg ]) ? true
    : (t == [ Pos, Pos ]) ? x.i > y.i
    : (t == [ Neg, Pos ]) ? false
    : x.i < y.i);
}

export const ige = (x, y) => igt(x, y) || x == y;
export const ilt = (x, y) => !igt(x, y) && x != y;
export const ile = (x, y) => !igt(x, y);
export const ieq = (x, y) => x == y;
export const ine = (x, y) => x != y;

export const iadd = (x, y) => {
  if (x.sign == y.sign) {
    return int(x.sign, x.i + y.i);
  } else {
    const [ max, min ] =
      (x.i > y.i) ? [ x, y ] : [ y, x ];
    return int(max.sign, max.i - min.i);
  }
}

export const isub = (x, y) => iadd (x, int(!y.sign, y.i));
export const imul = (x, y) => int(x.sign == y.sign, x.i * y.i);
export const idiv = (x, y) => int(x.sign == y.sign, x.i / y.i);
export const imod = (x, y) => isub(x, imul(idiv(x, y), y));

export const FixedPoint = Object({ sign: Bool, i: Object({ scale: UInt, i: UInt }) });

export const fx = (scale) => (sign, i) =>
  ({sign, i: { scale, i }});

export const fxint = (i) =>
  ({ sign: i.sign, i: { i: i.i, scale: 1 }})

const fxi2int = (x) =>
  int(x.sign, x.i.i);

export const fxrescale = (x, scale) => {
  if (x.i.scale == scale) {
    return x
  } else {
    const r = idiv( imul( fxi2int(x), + scale ), int(Pos, x.i.scale) );
    return fx(scale)(r.sign, r.i);
  }
}

export const fxunify = (x, y) => {
  const scale = x.i.scale < y.i.scale ? y.i.scale : x.i.scale;
  const x_ = fxrescale(x, scale);
  const y_ = fxrescale(y, scale);
  return [ scale, x_, y_ ];
}

export const fxadd = (x, y) => {
  const [ scale, x_, y_ ] = fxunify(x, y);
  const r = iadd( fxi2int(x_), fxi2int(y_) );
  return fx(scale)(r.sign, r.i);
}

export const fxsub = (x, y) => {
  const [ scale, x_, y_ ] = fxunify(x, y);
  const r = isub( fxi2int(x_), fxi2int(y_) );
  return fx(scale)(r.sign, r.i);
}

export const fxmul = (x, y) => {
  const r = imul( fxi2int(x), fxi2int(y) );
  return fx(x.i.scale * y.i.scale )(r.sign, r.i);
}

export const fxdiv = (x, y, scale_factor) => {
  const x_ = {
    i: imul( fxi2int(x), + scale_factor),
    scale: x.i.scale * scale_factor
  };
  const r = idiv( x_.i, fxi2int(y) );
  return fx(x_.scale / y.i.scale)(r.sign, r.i);
}

export const fxsqrt = (x, k) => {
  assert(x.sign == Pos, "fxsqrt: Cannot find the square root of a negative number.");
  return fx( x.i.scale / sqrt(x.i.scale, k) )(Pos, sqrt(x.i.i, k));
}

export const fxcmp = (cmp, x, y) => {
  const [ _, x_, y_ ] = fxunify(x, y);
  return cmp( fxi2int(x_), fxi2int(y_) );
}

export const fxlt = (x, y) => fxcmp(ilt, x, y);
export const fxle = (x, y) => fxcmp(ile, x, y);
export const fxgt = (x, y) => fxcmp(igt, x, y);
export const fxge = (x, y) => fxcmp(ige, x, y);
export const fxeq = (x, y) => fxcmp(ieq, x, y);
export const fxne = (x, y) => fxcmp(ine, x, y);

export const fxpowui = (base, power, precision) =>
  Array.iota(precision)
    .reduce([ +1.0, power, base ], ([ r, p, b ], _) =>
      [ (p % 2 == 1) ? fxmul(r, b) : r, p / 2, fxmul(b, b) ])
  [0];

export const fxpowi = (base, power, precision) => {
  const r = fxpowui(base, power.i, precision);
  return (power.sign)
    ? r
    : fxdiv(1, r, base.scale);
}

export const fxmod = (x, y) => {
    const [ _, x_, y_] = fxunify(x, y);
    const q = fxdiv(x_, y_, 1);
    const p = fxmul(q, y_);
    return fxsub(x_, p);
  }

export const fxfloor = (x) =>
  x.sign
  ? int(x.sign, fxrescale(x, 1).i.i)
  : int(x.sign, fxrescale(x, 1).i.i + 1);

const fxpow_ratio = (x, numerator, denominator, precision, scalePrecision) => {
  const xN = fxpowi(x, numerator, precision);
  const fxd = fxint(denominator);
  return Array.iota(precision).reduce(xN, (acc, _) => {
    const n = fxsub(fxpowi(acc, denominator, precision), xN);
    const d = fxmul(fxd, fxpowi(acc, isub(denominator, +1), precision));
    const t = fxdiv(n, d, 10);
    return fxrescale(fxsub(acc, t), scalePrecision);
  });
}

const getNumDenom = (value, precision) => {
  const [ numerator, denominator, _ ] =
    Array.iota(precision)
      .reduce([ +0, +1, value ], ([ accNum, accDen, accVal ], _) => {
        const i   = fxrescale(accVal, 1);
        const v   = fxsub(accVal, i);
        const num = iadd(accNum, fxi2int(i) );
        const v2  = fxmul(v, +2.0);
        return [ imul(num, +2), imul(accDen, +2), v2 ];
      });

  const [ hi, lo ] = (igt(numerator, denominator))
      ? [ numerator, denominator ]
      : [ denominator, numerator ];

  const [ _, _, lo_ ] = Array.iota(precision)
    .reduce([ false, hi, lo ], ([ br, accHi, accLo ], _) => {
      if (br) {
        return [ br, accHi, accLo ];
      } else {
        const rem = imod(accHi, accLo);
        return (rem.i == 0)
          ? [ true, accHi, accLo ]
          : [ false, accLo, accHi ];
      }
    });

  return [ idiv(numerator, lo_), idiv(denominator, lo_) ];
}

export const fxpow = (base, power, precision, scalePrecision) => {
  const whole = fxfloor(power);
  const fwhole = fxint(whole);
  if (fxeq(power, fwhole)) {
    return fxpowi(base, whole, precision);
  } else {
    const remain = fxsub(power, fwhole);
    const wholePow = fxpowi(base, whole, precision);
    const [ num, den ] = getNumDenom(remain, precision);
    return fxmul(wholePow, fxpow_ratio(base, num, den, precision, scalePrecision));
  }
}

export const pow = (base, power, precision) =>
  Array.iota(precision)
    .reduce([ 1, power, base ], ([ r, p, b ], _) =>
      [ (p % 2 == 1) ? r * b : r, p / 2, b * b ])
  [0];
