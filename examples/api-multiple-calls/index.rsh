'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  const B = API({
    go: Fun([], Null),
  });
  init();
  A.publish();
  commit();

  const [ [], k1 ] = call(B.go);
  k1(null);
  commit();

  const [ [], k2 ] = call(B.go);
  k2(null);
  commit();
});
