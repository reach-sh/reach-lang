'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { x: UInt });
  setOptions({ verifyArithmetic: true });
  deploy();

  A.only(() => {
    const x = declassify(interact.x);
    assume(x == 1);
  });

  A.publish(x)
    .payRequire(() => {
      require(x == 1);
    })
    .pay(x);

  transfer(1).to(A);
  commit();

});
