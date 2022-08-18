'reach 0.1';

const BytesDyn = Bytes(128);

export const main = Reach.App(() => {
  setOptions({ connectors: [ ETH ] });
  const A = Participant('A', { t: BytesDyn });
  const B = Participant('B', { chk: Fun([BytesDyn], Null) });
  init();
  A.only(() => { const t = declassify(interact.t); });
  A.publish(t);
  B.interact.chk(t);
  commit();
  exit();
});
