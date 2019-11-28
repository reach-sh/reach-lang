'reach 0.1 exe';

function main() {
  A.pay(0)
    .timeout(DELAY, 7, () => {
      return false; });
  return true; }
