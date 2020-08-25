'reach 0.1 exe';

const A = newParticipant();
const DELAY = 10;
const AMOUNT = 10;

function main() {
  A.pay(AMOUNT);

  var [ i ] = [ 0 ];
  invariant(true);
  while (i < 2) {
    commit();
    interact.helloOuter(i);
    A.pay(AMOUNT);

    var [ j ] = [ 0 ];
    invariant(true);
    while (j < i) {
      commit();
      interact.helloInner(i, j);
      A.pay(AMOUNT);

      [ j ] = [ j + 1 ];
      continue;
    }

    [ i ] = [ i + 1 ];
    continue;
  }
  transfer(balance()).to(A);
  commit();
  return 0;
}
