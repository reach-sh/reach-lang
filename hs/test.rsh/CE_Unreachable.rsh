'reach 0.1 exe';

const A = newParticipant();

function main() {
  A.only(() => {
    const x = is(uint256, interact.getX());
    if ( x == 0 ) {
      if ( x > 0 ) {
        interact.here(0); }
      else {
        interact.here(1); } }
    else {
      interact.here(2); } });
  return 0; }
