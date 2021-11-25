'reach 0.1';

const Part = {
  ...hasConsoleLogger,
  deadline: UInt,
};

export const main = Reach.App(() => {
  const A = Participant('Alice', Part);
  const B = Participant('Bob', Part);
  deploy();

  A.only(() => {
    const deadline = declassify(interact.deadline);
  });
  A.publish(deadline);
  commit();
  B.publish();
  each([A, B], () => { interact.log('start',0); });

  const [tr, kg] = makeDeadline(deadline);
  const x = parallelReduce(0)
    .invariant(balance() == 0)
    .while(kg())
    .case(A, () => ({}), (_) => {
      each([A, B], () => { interact.log('while-after',x); });
      return x + 1;
    })
    .timeRemaining(tr());
  commit();
  each([A, B], () => { interact.log('end',x); });
  exit();
});
