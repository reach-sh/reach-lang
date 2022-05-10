'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    log: Fun(true, Null),
  });
  const B = API({
    go : Fun([Maybe(UInt)], Bool),
  });
  init();

  A.publish();
  commit();

  const [ [mi], k ] = call(B.go);

  switch (mi) {
    case Some: {
      A.interact.log("Some");
      k(false);
    }
    case None: {
      A.interact.log("None");
    }
  }

  commit();

});
