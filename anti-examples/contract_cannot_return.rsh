'reach 0.1 exe'

const A = participant({});

function main() {
  A.pay(1);
  transfer(balance()).to(A);
}
