'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    token           : Token,
    requestMoney    : Fun([Address], Null),
    triggerClawback : Fun([Address, UInt], Null),
    checkBal        : Fun([Address, UInt], Null),
    ...hasConsoleLogger
  });
  init();

  A.only(() => {
    const token = declassify(interact.token);
  });
  A.publish(token);
  commit();

  const addr = getAddress();
  A.interact.requestMoney(addr);
  A.interact.checkBal(addr, 0);

  A.publish();
  commit();

  const funds = getUntrackedFunds(token);
  A.interact.triggerClawback(addr, funds);
  A.interact.checkBal(addr, 1);

  A.publish();
  commit();

  A.interact.checkBal(addr, 2);

  A.publish();
  transfer(funds, token).to(A);
  commit();

});
