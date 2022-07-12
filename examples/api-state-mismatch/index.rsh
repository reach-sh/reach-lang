'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    done: Fun([], Null)
  });
  const B = API('B', {
    f: Fun([], Null),
    g: Fun([], Null),
  })
  init();

  A.publish();
  A.interact.done();
  commit();

  const [ _, k1] = call(B.f);
  k1(null);
  commit();

  const [ _, k2 ] = call(B.g);
  k2(null);
  commit();

  const [ _, k3 ] = call(B.f);
  k3(null);
  commit();

});
