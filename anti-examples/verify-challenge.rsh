'reach 0.1 exe';

const A = participant({});

function main() {
  A.only(() => {
    const x = declassify(is(uint256, interact.getX()));
    assume(x == 1); });
  A.publish(x);
  require(x == 1);
  
  var [ go ] = [ true ];
  invariant(balance() == 0);
  while ( go ) {
    commit();

    A.publish();

    [ go ] = [ false ];
    continue; }

  commit();

  assert(x == 1);

  return; }
