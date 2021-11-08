'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  deploy();
  A.publish();
  A.only(() => {
    assert(didPublish());
  });
  commit();
  exit();
});
