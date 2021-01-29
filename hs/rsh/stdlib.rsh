'reach 0.1';

export function not (x) {
  return (x ? false : true); }
export const boolEq = (x, y) => (x ? y : !y);

// Operator abbreviation expansions
export function minus (x) {
  return 0 - x; }
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

const sqrt = (y, k) =>
  Array.iota(k).reduce([ y, (y / 2 + 1) ], ([ z, x ], a) =>
    (x < z)
      ? [ x, ((y / x + x) / 2) ]
      : [ z, x ]
  )[1];
