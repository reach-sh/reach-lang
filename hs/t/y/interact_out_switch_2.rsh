'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    log: Fun(true, Null),
    mi: Maybe(UInt)
  });
  const B = API({
    go : Fun([Maybe(UInt)], Bool),
  });
  init();

  A.only(() => {
    const mi = declassify(interact.mi);
  });
  A.publish(mi);


  switch (mi) {
    case Some: {
      A.interact.log("Some");
      commit();
      const [ [mx], k ] = call(B.go);
      k(false);
    }
    case None: {
      A.interact.log("None");
    }
  }

  commit();

});
