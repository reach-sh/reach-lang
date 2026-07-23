'reach 0.1';

// A minimal "verified Solidity" contract: a payable deposit that can only be
// withdrawn, in full, by the original depositor.  Compile it with
// `reach sol` (or `reachc --sol`) to get plain Solidity + ABI + a
// machine-readable verification report; the Z3 checks (balance sufficiency,
// token linearity, overflow, assertion honesty) run on every compile.

export const main = Reach.App(() => {
  setOptions({ connectors: [ETH] });

  const Depositor = Participant('Depositor', {
    amt: UInt,
    ready: Fun([], Null),
  });

  init();

  Depositor.only(() => {
    const amt = declassify(interact.amt);
  });
  Depositor.publish(amt).pay(amt);
  commit();

  Depositor.only(() => {
    interact.ready();
  });
  Depositor.publish();
  transfer(amt).to(Depositor);
  commit();

  exit();
});
