'reach 0.1 exe';

const A = participant({});
const B = participant({});

function main() {
  A.only(() => {
    const y = 2; });
  B.publish(y).timeout(DELAY, _, () => {
    commit();
    return 1; });
  return 0; }
