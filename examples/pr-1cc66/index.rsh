'reach 0.1';
export const main = Reach.App(() => {
  const Alice = Participant('Alice', { pmt: UInt });
  deploy();
  Alice.only(() => {
    const pmt = declassify(interact.pmt); });
  Alice.publish(pmt).pay(pmt);
  var [ claimed_by, keep_going ] = [ Maybe(Address).None(), true ];
  invariant(balance() == pmt);
  while (keep_going) {
    commit();
    Alice.publish();
    keep_going = false;
    continue;
  }
  transfer(balance()).to(fromSome(claimed_by, Alice));
  commit();
  exit();
});
