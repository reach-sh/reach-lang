'reach 0.1 exe';

const A = participant({});

function main() {
  A.pay(0).timeout(0, _, () => { commit(); return 0; });
  const x = random();
  commit();
  return 1; }
