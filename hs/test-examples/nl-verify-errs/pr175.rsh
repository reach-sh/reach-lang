'reach 0.1';
export const main = Reach.App(() => {
  const Alice = Participant('Alice', { });
  deploy();
  function attempt() {
    if (true) { return 1; }
    return 0;
  }
  assert(attempt() == 0);
  exit();
});
