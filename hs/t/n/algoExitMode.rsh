'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ ALGOExitMode: 'Bad' });
  const A = Participant('A', {
  });
  init();
  A.publish();
  commit();
  exit();
});
