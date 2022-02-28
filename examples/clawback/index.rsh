'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    token           : Token,
    amt             : UInt,
    triggerClawback : Fun([Address, UInt], Null),
    checkBal        : Fun([Address, UInt], Null),
    ...hasConsoleLogger
  });
  init();

  A.only(() => {
    const token = declassify(interact.token);
    const amt   = declassify(interact.amt);
  });
  A.publish(token, amt);
  commit();

  A.pay([[amt, token]]);
  const addr = getAddress();
  A.interact.checkBal(addr, 0);
  // expBal == amt
  commit();

  A.interact.triggerClawback(addr, amt);
  // expBal == 0

  void getUntrackedFunds(token);
  // expBal == 0  (does not overflow)

  A.interact.checkBal(addr, 1);
  // expBal == 0

  A.publish();
  // This fails cause we don't have a balance of `amt`
  transfer(balance(token), token).to(A);
  commit();

});
