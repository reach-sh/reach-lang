'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    go: Fun([], Bool),
  });
  const B = API({
    go: Fun([], Bool),
  });
  deploy();

  A.publish();
  commit();

  const [ [], k ] = call(B.go);
  k(true);
  commit();

  A.only(() => {
    const x = declassify(interact.go());
  });
  A.publish();

  commit();
  exit();
});
