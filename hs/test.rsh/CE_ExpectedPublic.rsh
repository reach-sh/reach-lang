'reach 0.1 exe';

const A = newParticipant();

function main() {
  A.only(() => {
    const x = is(uint256, interact.getX()); });
  A.publish(x);
  commit();
  return 1; }
