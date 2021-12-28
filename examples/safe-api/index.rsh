'reach 0.1';

const MBool = Maybe(Bool);
export const main = Reach.App(() => {
  const A = Participant('Alice', {
    launchBob: Fun([UInt, MBool], Null),
  });
  const B = API({
    go: Fun([], Bool),
  });
  init();

  A.publish();
  commit();
  A.interact.launchBob(0, MBool.Some(true));

  const [ [], k ] = call(B.go);
  k(true);
  commit();

  A.interact.launchBob(1, MBool.None());
  A.publish();

  commit();
  exit();
});
