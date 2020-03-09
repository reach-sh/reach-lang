'reach 0.1 lib';

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
function implies (x, y) {
  return (not(x) || y); };

function ensure(f, x) {
  assert(f(x));
  return x; };

function precommit (x) {
  const salt = random();
  const commitment = digest(salt, x);
  return [commitment, salt]; }

function check_commit (commitment, salt, x) {
  return require(commitment == digest(salt, x)); }

function __decode_testing__() {
  return txn.value; }
