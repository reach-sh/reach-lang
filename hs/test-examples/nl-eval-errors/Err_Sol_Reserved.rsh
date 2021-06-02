'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], Struct([
      ['super', Address]
    ])),
    show: Fun([Address], Null) });
  deploy();
  A.only(() => {
    const o = declassify(interact.get());
    assume(o.super == A);
  });
  A.publish(o);
  const a = o.super;
  require(a == A);
  commit();
  A.interact.show(o.super);
});
