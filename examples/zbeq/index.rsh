// index.rsh
'reach 0.1';

const B9 = Bytes(9);
const BA = Bytes(10);
export const main = Reach.App(() => {
  const A = Participant('Alice', {
    get9: Fun([], B9),
    getA: Fun([], BA),
    put: Fun([BA, BA, Bool], Null),
    check: Fun(true, Null),
  });
  const vD = View('Debug', {
    vals: Tuple(BA, B9, BA, Bool, Bool, Bool),
  });
  deploy();
  A.only(() => {
    const x = declassify(interact.getA());
    const y = declassify(interact.get9());
  });
  A.publish(x, y);
  const e = BA.pad('');
  const yp = BA.pad(y);
  const xx = x == x;
  const xy = x == yp;
  const xe = x == e;
  A.interact.put(x, x, xx);
  A.interact.put(x, yp, xy);
  A.interact.put(x, e, xe);
  const vals = [x, y, yp, xx, xy, xe];
  vD.vals.set(vals);
  commit();
  A.interact.check(vals);

  A.publish();
  commit();

  exit();
});
