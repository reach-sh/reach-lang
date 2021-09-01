'reach 0.1';

const Log1 = {
    m1: Fun([Address, UInt], Null),
}; 
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    get: Fun([], Tuple(Address, UInt)),
  });
  const Bob = Participant('Bob', {
    check: Fun([Address, UInt], Null),
  });
  deploy();
    Alice.only(() => {
      const [acct, amt] = declassify(interact.get());
    });
    Alice.publish(acct, amt);
    const log1 = remote(acct, Log1);
    log1.m1(acct, amt);
    commit();
    Bob.interact.check(acct, amt);
    exit();
  });
