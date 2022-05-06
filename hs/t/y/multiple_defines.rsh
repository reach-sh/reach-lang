'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API({
    f: Fun([UInt], UInt),
    g: Fun([UInt], UInt)
  })
  init();

  A.publish();

  const [ keepGoing ] =
    parallelReduce([ true ])
      .invariant(balance() == 0)
      .while(keepGoing)
      .define(() => {
        const mul2 = (x) => x * 2;
      })
      .api_(B.f,
        (x) => {
          return [ (k) => {
            k(mul2(x));
            return [ false ];
          }];
        })
      .define(() => {
        const div2 = (x) => mul2(x) / 2 / 2;
      })
      .api_(B.g,
        (x) => {
          return [ (k) => {
            k(div2(mul2(x)));
            return [ false ];
          }];
        })
      .timeout(false);

  commit();
})
