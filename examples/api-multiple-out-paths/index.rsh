'reach 0.1';

const Effect = Fun(true, Null);

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Effect,
    log: Effect,
  });
  const B = API({
    go : Fun([UInt], Bool),
  });
  init();

  A.publish();
  A.interact.deployed();
  commit();

  const [ [i], k ] = call(B.go);

  if (i > 5) {
    A.interact.log("i > 5");
    k(true);
  } else {
    A.interact.log("i <= 5");
    k(false);
  }
  commit();

});
