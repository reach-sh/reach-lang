'reach 0.1';

export const main = Reach.App(() => {

  const A = Participant('A', {
    when: Fun([], Bool),
    x: UInt,
  });

  init();

  A.only(() => {
    const _x = interact.x;
    const when = declassify(interact.when());
    const o = { _x, when };
    const r = when ? o : o;
    const _xp = r._x;
  });

});
