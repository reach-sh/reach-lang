'reach 0.1';
const LogAttack2 = {
    m2: Fun([], Null),
};
export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    get: Fun([], Tuple(Contract, UInt)),
  });
  const Bob = Participant('Bob', {
    check: Fun([UInt], Null),
  });
  deploy();
  Alice.publish();
  commit();
  Alice.only(() => {
    const [acct, val] = declassify(interact.get());
  });
  Alice.publish(acct);
  const log2 = remote(acct, LogAttack2);
  log2.m2();
  commit();
  Alice.publish(val);
  commit();
  Bob.interact.check(val);
  exit();
});
