'reach 0.1';

export const main = Reach.App(() => {
  const iface = { post: Fun([], Bytes(4)) };
  const A = Participant('A', iface);
  const B = Participant('B', iface);
  const E = Events({ e: [Bool] });
  init();
  A.only(() => { const v1 = declassify(interact.post()); });
  A.publish(v1);
  E.e(BytesDyn(v1) == BytesDyn("hiya"))
  commit();
  B.only(() => { const v2 = declassify(interact.post()); });
  B.publish(v2);
  E.e(BytesDyn(v2) == BytesDyn("goodbye"))
  commit();
});
