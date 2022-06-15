'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API({
    a: Fun([Address, Bool], Address),
    b: Fun([Address, UInt], UInt),
  }, {
    a: 'c',
  });
  const C = API({
    d: Fun([Address, UInt], UInt),
  }, {
  });
  init();
  A.publish();
  commit();
  const [ _, k ] = call(B.a);
  k(this);
  commit();
  const [ _, k1 ] = call(B.b);
  k1(1);
  commit();
  const [ _, k2 ] = call(C.d);
  k2(1);
  commit();
});
