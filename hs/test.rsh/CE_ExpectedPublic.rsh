'reach 0.1 exe';

const A = participant({x: uint256});

function main() {
  A.publish(x).timeout(0, _, () => { commit(); return 0; });
  commit();
  return 1; }
