'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { ...hasConsoleLogger });
  init();
  A.only(() => {
    interact.log(didPublish());
  });
  A.publish();
  commit();
  exit();
});

