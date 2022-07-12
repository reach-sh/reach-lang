'reach 0.1';
export const main = Reach.App(() => {
  const C = Participant("C", {});
  const P = API({
    f: Fun([UInt], Null),
  });
  init();
  C.publish();
  commit();
  const [a] = parallelReduce([0])
    .while(keepGoing())
    .api(
      (amount) => {
      },
      (amount) => amount,
      (amount, callback) => {
        return [a];
      }
    );
  C.publish();
  commit();
});
