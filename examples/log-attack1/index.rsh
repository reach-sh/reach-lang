'reach 0.1';

const ERC20 = {
    m2: Fun([Address, UInt], Null),
}; 
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...hasConsoleLogger,
    m2: Fun([], Tuple(Address, UInt)),
  });
  deploy();
    Alice.only(() => {
      const [acct, amt] = declassify(interact.m2()); 
    });
    Alice.publish(acct, amt);
    commit();
    Alice.publish();
    const mint = remote(acct, ERC20);
    mint.m2(acct, amt);
    commit();
    Alice.interact.log('log-attack1 matching sol+reach event logs deployed');
    exit();
  });
