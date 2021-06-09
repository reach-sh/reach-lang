'reach 0.1';
export const main = Reach.App(() => {
  const Alice = Participant('Alice', { assertEq: Fun(true, Null) });
  deploy();
  function attempt() {
    if (true) { return 1; }
    return 0;
  }
  assert(attempt() == 1);
  Alice.interact.assertEq(attempt(), 1);
  exit();
});
