'reach 0.1 exe';

const A = newParticipant();

function main() {
  A.only(() => {
    const len = is(uint256, interact.getLen());
    const xs = is(uint256[len], interact.getArr(len));
  });
  return;
}
