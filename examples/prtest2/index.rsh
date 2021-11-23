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

  const x = parallelReduce(0)
    .invariant(balance() == 0)
    .while( x < 5 )
    .case(A, () => ({}), (_) => {
      each([A, B], () => { interact.log('while-after',x); });
      return x + 1;
    });
  commit();
  each([A, B], () => { interact.log('end',x); });
  exit();
});
