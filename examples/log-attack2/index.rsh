'reach 0.1';
const LogAttack2 = {
    m2: Fun([Address, UInt], Null),
    m3: Fun([UInt], Null),
}; 
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...hasConsoleLogger,
    m2: Fun([], Tuple(Address, UInt)),
    m3: Fun([], UInt),
  });
  deploy();
    Alice.only(() => {
      const [acct, amt] = declassify(interact.m2()); 
      const date = declassify(interact.m3()); 
    });
    Alice.publish(acct, amt, date);
    commit();
    Alice.publish();
    const log2 = remote(acct, LogAttack2);
    log2.m2(acct, amt);
    log2.m3(date);
    Alice.interact.log(
      "MATCHING LOG W SOLIDITY e2:msg.sender", acct, "msg.value", amt);
    Alice.interact.log("MATCHING e3 EVENT : msg.sender", acct,"DATE" ,date, "msg.value",amt);
    commit();
    exit();
  });