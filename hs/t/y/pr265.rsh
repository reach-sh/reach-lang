'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {});
  deploy();

  A.publish()
    .timeout(5, () => {
      exit();
    });
  commit();
  exit();
});
