'reach 0.1';

export const main = Reach.App(() => {

  const A = Participant('A', {
    when: Fun([], Bool),
    when2: Fun([], Bool),
    b: Bool,
    x: UInt,
    seeP: Fun([UInt], Null)
  });

  init();

  const LD = Data({ LA: UInt, LB: Bool });
  const MD = Data({ MA: Null, MB: Null });
  A.only(() => {
    const run1 = () => {
      const when = declassify(interact.when());
      const _local = LD.LA(interact.x);
      const r = { _local, when, msg: MD.MA(null) };
      return r;
    };
    const run2 = () => {
      const when = declassify(interact.when2());
      const _local = LD.LB(interact.b);
      const r = { _local, when, msg: MD.MB(null) };
      return r;
    };

    const r1 = run1();
    const r2 = run2();
    const r = r1.when ? r1 : r2;
    const o_msg = r.msg;
    // assume(r.msg.match({ MA: _ => true, default: _ => false }) ? r.local.match({ LA: _ => true, default: _ => false }) :
    //        r.msg.match({ MB: _ => true, default: _ => false }) ? r.local.match({ LB: _ => true, default: _ => false }) : true);
  });
  A.publish(o_msg);
  switch(o_msg) {
    case MA: {
      A.only(() => {
        const _x = r._local.match({
          LA: (_l) => { return _l; },
          default: (_) => { return 0; }
        });
      });
      A.only(() => {
        interact.seeP(_x);
      })
    }
    case MB: {

    }
  };
  commit();

});
