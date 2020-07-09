'reach 0.1 exe'

const isEyewear = Enum([SUNGLASSES, GLASSES, GOGGLES]);

const aBool = SUNGLASSES == 0;  // what does comparing an enum to ints mean?

const A = participant({});

function main() {
  if (aBool) {
    interact.wat();
  } else {
    return; // what does empty return mean? Why require an else branch if you can just return; ?
  }
  A.pay(1);
  transfer(balance()).to(A);
  commit();
  return 0;
}
