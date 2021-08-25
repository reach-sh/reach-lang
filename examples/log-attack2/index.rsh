'reach 0.1';
const LogAttack2 = {
    m1: Fun([Address, UInt], Null),
    m2: Fun([UInt], Null),
}; 
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...hasConsoleLogger,
    m1: Fun([], Tuple(Address, UInt)),
    m2: Fun([], UInt),
  });
  const Bob = Participant('Bob', {
    ...hasConsoleLogger,
  });
    deploy();
    Alice.only(() => {
      const [acct, amt] = declassify(interact.m1()); 
      const date = declassify(interact.m2()); 
    });
    
    Alice.publish(acct, amt);
    const log2 = remote(acct, LogAttack2);
    log2.m1(acct, amt);
    commit();

    Alice.publish(date);
    log2.m2(date);
    Alice.interact.log(["Alice sees early event2", date]);     
    Bob.interact.log(["Bobs sees early event2", date]);
    commit();
    exit();
  });