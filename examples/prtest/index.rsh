'reach 0.1';

const Part = {
  ...hasConsoleLogger,
};

export const main = Reach.App(() => {
  const A = Participant('Alice', Part);
  const B = Participant('Bob', Part);
  deploy();

  A.publish();
  commit();
  B.publish();
  each([A, B], () => { interact.log('start',0); });

  var x = 0;
  invariant(balance() == 0);
  while ( x < 5 ) {
    commit();
    each([A, B], () => { interact.log('while-before',x); });
    A.publish();
    each([A, B], () => { interact.log('while-after',x); });
    x = x + 1;
    continue;
  }
  commit();
  each([A, B], () => { interact.log('end',x); });
  exit();
});
