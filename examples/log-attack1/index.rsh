'reach 0.1';

const Log1 = {
    m2: Fun([Address, UInt], Null),
}; 
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...hasConsoleLogger,
    m2: Fun([], Tuple(Address, UInt)),
  });
  const Bob = Participant('Bob', {
    ...hasConsoleLogger,
  });
  deploy();
    Alice.only(() => {
      const [acct, amt] = declassify(interact.m2()); 
    });
    Alice.publish(acct, amt);
    const log1 = remote(acct, Log1);
    log1.m2(acct, amt);
    Alice.interact.log(["Alice sees Alice's address:", acct, "Alice Amount", amt]);
    Bob.interact.log(["Bob sees Alice's address:", acct, "Alice Amount", amt]);
    commit();
    exit();
  });