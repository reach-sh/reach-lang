'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun([], Null),
  });
  const T = API('Bob', {
    add1: Fun([UInt], UInt),
  });
  const U = API({
    add1: Fun([UInt], UInt),
  });
  init();
  A.publish();
  commit();
  A.interact.deployed();
  const [ [x], k] = call(U.add1);
  k(x + 1);
  commit();

  const [ [y], k2] = call(T.add1);
  k2(y + 1);
  commit();

});
