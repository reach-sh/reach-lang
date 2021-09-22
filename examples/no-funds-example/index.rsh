'reach 0.1';

export const main = Reach.App(() => {
  const Deployer = Participant('Deployer', {deadline: UInt});
  const Attacher = Participant('Attacher', {});
  setOptions({deployMode: 'firstMsg'});
  deploy();
  Deployer.only(() => {
    const deadline = declassify(interact.deadline);
  })
  Deployer.publish(deadline);
  commit();
  Attacher.publish().timeout(deadline, () => {
    Anybody.publish();
    commit();
    exit();
  });
  commit();
});
