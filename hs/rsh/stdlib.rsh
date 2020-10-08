'reach 0.1';

export function not (x) {
  return (x ? false : true); }
export const boolEq = (x, y) => (x ? y : !y);

// Polymorphic eq made from builtins
export const polyEq = (x, y) => {
  const ty_x = typeOf(x);
  // TODO: add structural equality for arrays and objects
  if (typeEq(ty_x, Bool)) {
    return boolEq(x, y);
  } else if (typeEq(ty_x, Bytes)) {
    return bytesEq(x, y);
  } else if (typeEq(ty_x, Digest)) {
    return digestEq(x, y);
  } else if (typeEq(ty_x, Address)) {
    return addressEq(x, y);
  } else if (isType(x)) {
    return typeEq(x, y);
  } else {
    return intEq(x, y);
  }
};

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

// Standard library functions that should be hidden in some way, like
// SLV_HaskellFunction FIXME

export function Object_set(o, k, e) {
  return {...o, [k]: e};
}
export const Array_empty =
  Array.iota(0);
export const Array_replicate =
  (sz, v) => Array.iota(sz).map(x => v);
