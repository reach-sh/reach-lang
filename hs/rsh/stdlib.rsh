'reach 0.1 lib';

// Operator abbreviation expansions
function not (x) {
  return (x ? false : true); };
function neq (x, y) {
  return not(x == y); };
function bytes_neq (x, y) {
  return not(x === y); };
function or (x, y) {
  return (x ? true : y); };
function and (x, y) {
  return (x ? y : false); };

// Library functions
function implies (x, y) {
  return (not(x) || y); };

function ensure(f, x) {
  assert(f(x));
  return x; };

function makeCommitment (x) {
  const salt = random();
  const commitment = digest(salt, x);
  return [commitment, salt]; }

function checkCommitment (commitment, salt, x) {
  return require(commitment == digest(salt, x)); }

function closeTo(Who, result) {
  return () => {
    Who.publish();
    transfer(balance()).to(Who);
    commit();
    return result; }; }

function __decode_testing__() {
  return txn.value; }
