'reach 0.1';

const TypeWithSolKwd = Object({
  address: Address
});

export const main = Reach.App(() => {
  const A = Participant('A', {
    get: Fun([], TypeWithSolKwd),
    show: Fun([Address], Null) });
  deploy();
  A.only(() => {
    const o = declassify(interact.get());
    assume(o.address == A);
  });
  A.publish(o);
  const a = o.address;
  require(a == A);
  commit();
  A.interact.show(o.address);
});
