'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ connectors: [ETH] });
  const A = Participant('Alice', {});
  const B = API({
    go: Fun([], Null),
  });
  init();
  A.publish();
  commit();

  const [ _, k1 ] = call(B.go);
  k1(null);
  commit();

  const [ _, k2 ] = call(B.go);
  k2(null);
  commit();
});
