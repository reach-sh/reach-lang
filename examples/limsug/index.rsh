'reach 0.1';

export const main = Reach.App(() => {
  const Driver = Participant('Driver', {i: UInt, ready: Fun([], Null)});
  const A = API({halt: Fun([], Null)});
  const V = View({Info: UInt});
  init();
  Driver.publish();
  commit();
  Driver.only(() => { const i = declassify(interact.i); });
  Driver.publish(i);
  V.Info.set(i);
  commit();
  Driver.interact.ready();
  const [_, k] = call(A.halt);
  k(null);
  commit();
});
