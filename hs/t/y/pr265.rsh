'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  init();
  A.publish();
  commit();
  A.publish()
    .timeout(relativeTime(5), () => {
      exit();
    });
  commit();
  exit();
});
