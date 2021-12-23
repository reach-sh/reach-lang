'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();
  A.publish();
  A.only(() => {
    assert(didPublish());
  });
  commit();
  exit();
});
