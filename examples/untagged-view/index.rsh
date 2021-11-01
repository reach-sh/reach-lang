'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    x: Fun([], UInt),
    view: Fun([], Null),
  });
  const VU = View({ xv: UInt });
  const VT = View('V', { xv: UInt });
  deploy();

  A.only(() => {
    const x = declassify(interact.x());
  });
  A.publish(x);

  var [ xp ] = [ x ]
  {
    VU.xv.set(xp);
    VT.xv.set(xp);
  }
  invariant(balance() == 0);
  while (xp < 5) {
    commit();

    A.only(() => {
      const nx = declassify(interact.x());
      interact.view();
    });
    A.publish(nx);

    [ xp ] = [ nx ];
    continue;
  };
  commit();

});
