'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  init();

  A.publish();
  var x = 0;
  while ( x < 5 ) {
    commit();
    A.publish();
    x = x + 1;
    continue;
  }
  commit();
})
