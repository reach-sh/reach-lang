'reach 0.1 exe';

function main() {
  const A = 1;
  A.pay(42).timeout(10, _, () => { return false; });
  return true; }
