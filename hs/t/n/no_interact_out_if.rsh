'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    log: Fun(true, Null),
  });
  const B = API({
    go : Fun([UInt], Bool),
  });
  init();

  A.publish();
  commit();

  const [ [i], k ] = call(B.go);

  if (i > 5) {
    A.interact.log("i > 5");
  } else {
    A.interact.log("i <= 5");
    k(false);
  }
  commit();

});
