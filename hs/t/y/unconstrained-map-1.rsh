'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();

  A.publish();
  const constrainedMap1 = new Map(Null);
  const constrainedMap2 = new Map(Null);
  const unconstrainedMap1 = new Map(Null);
  const unconstrainedMap2 = new Map(Null);

  var _ = null;
  invariant(constrainedMap1.size() == 0 && constrainedMap2.size() == 0 && balance() == 0);
  while (true) {
    commit();
    exit();
  }

  assert(false);
  commit();
  exit();
});
