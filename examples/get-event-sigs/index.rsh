'reach 0.1';

export const main = Reach.App(() => {
  const Deployer = Participant('Deployer', {i: UInt});
  const N = Events({
    pub: [UInt],
    big: [Tuple(Maybe(Bool), Array(Bytes(2), 2)), Tuple(Address, Contract, UInt256, Digest)],
  });
  init();
  Deployer.only(() => { const i = declassify(interact.i); });
  Deployer.publish(i);
  N.pub(i);
  N.big([Maybe(Bool).Some(false), array(Bytes(2), ["hi", "ho"])], [this, getContract(), UInt256(42), digest(0)]);
  commit();
});
