'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();

  A.publish();
  const m = new Map(Null);

  var _ = null;
  invariant(m.size() == 0 && balance() == 0);
  while (false) {
    commit();
    exit();
  }

  var _ = null;
  invariant(m.size() == 0 && balance() == 0);
  while (false) {
    commit();
    exit();
  }

  var _ = null;
  invariant(balance() == 0);
  while (true) {
    commit();
    exit();
  }

  var _ = null;
  invariant(m.size() == 0 && balance() == 0);
  while (false) {
    commit();
    exit();
  }

  assert(false);
  commit();
  exit();
});
