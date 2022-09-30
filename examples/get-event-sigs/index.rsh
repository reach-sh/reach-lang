'reach 0.1';

export const main = Reach.App(() => {
  const Deployer = Participant('Deployer', {i: UInt});
  const N = Events({pub: [UInt]});
  init();
  Deployer.only(() => { const i = declassify(interact.i); });
  Deployer.publish(i);
  N.pub(i);
  commit();
});
