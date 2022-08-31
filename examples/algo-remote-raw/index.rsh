'reach 0.1';
'use strict';

export const client = Reach.App(() => {
  const A = Participant('Alice', {
    getCtc: Fun([], Contract),
    ...hasConsoleLogger,
  });
  init();

  A.only(() => {
    const ctc = declassify(interact.getCtc());
  });

  A.publish(ctc);
  // Reach publish format: [method, publish, time, msg]
  const r_ctc = remote(ctc, {bobPub: Fun([UInt, UInt, UInt, Tuple(UInt, UInt)], Null)});

  const ret = r_ctc.bobPub.ALGO({
    rawCall: true
  })(0, 1, 0, [255, 1]);
  void ret;
  commit();
  A.interact.log("remote call return:", ret);
  exit();
});

export const server = Reach.App(() => {
  const iface = {
    getInt: Fun([], UInt),
    ready: Fun([], Null),
    ...hasConsoleLogger,
  };
  const A = Participant('Alice', iface);
  const B = Participant('Bob', iface);
  init();

  A.only(() => {
    const x = declassify(interact.getInt());
  });
  A.publish(x);
  commit();
  A.interact.ready();
  B.only(() => {
    const y = declassify(interact.getInt());
    const z = declassify(interact.getInt());
  })
  B.publish(y,z);
  const sum = x + y + z;
  commit();
  A.interact.log("server sum:", sum);
  A.publish();
  commit();
  exit();
});
