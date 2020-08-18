'reach 0.1';

// Operator abbreviation expansions
export function minus (x) {
  return 0 - x; };
export function not (x) {
  return (x ? false : true); };
export function neq (x, y) {
  return not(x == y); };
export function bytes_neq (x, y) {
  return not(x === y); };
export function or (x, y) {
  return (x ? true : y); };
export function and (x, y) {
  return (x ? y : false); };

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
export const bytes_eq = (x, y) => x === y;
export const lsh = (x, y) => x << y;
export const rsh = (x, y) => x >> y;
export const band = (x, y) => x & y;
export const bior = (x, y) => x | y;
export const bxor = (x, y) => neq(x, y);

// Library functions
export function implies (x, y) {
  return (not(x) || y); };

export function ensure(f, x) {
  assert(f(x));
  return x; };

export const hasRandom = {
  random: Fun([], UInt256) };

export function makeCommitment (interact, x) {
  const salt = interact.random();
  const commitment = digest(salt, x);
  return [commitment, salt]; };

export function checkCommitment (commitment, salt, x) {
  return require(commitment == digest(salt, x)); };

export function closeTo(Who, after) {
  Who.publish();
  transfer(balance()).to(Who);
  commit();
  after();
  exit(); };

function __decode_testing__() {
  return txn.value; };
