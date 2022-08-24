'reach 0.1';

const B4  = Bytes(4);
const B32 = Bytes(32);
const B33 = Bytes(33);

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    deployed: Fun([], Null),
    getS: Fun([], B4),
    log: Fun(true, Null),
    s: B4,
    chkViews: Fun(true, Null),
  });

  const Api = API({
    f: Fun([B4], Bool)
  });

  const V = View({
    b32: B32,
    b33: B33
  });

  init();

  A.only(() => {
    const s = declassify(interact.s);
  });
  A.publish(s);
  A.interact.deployed();
  commit();

  // API method with `bytes4` in signature
  const [ [bs], k ] = call(Api.f);
  k(bs == s);
  commit();

  A.publish();
  var [x, i, b32, b33] = [s, 0, B32.pad(s), B33.pad(s)];
  {
    V.b32.set(b32);
    V.b33.set(b33);
  }
  invariant(balance() == 0);
  while (i < 10) {
    commit();

    A.only(() => {
      const sp = declassify(interact.getS());

      // Check that b32/b33 exists and is 32/33 bytes long
      interact.chkViews(i);
    });
    A.publish(sp);

    const b32p = B32.pad(sp);
    const b33p = B33.pad(sp);

    [x, i, b32, b33] = [sp, i + 1, b32p, b33p];
    continue;
  }

  commit();

});
