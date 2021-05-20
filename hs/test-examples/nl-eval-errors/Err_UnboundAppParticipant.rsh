'reach 0.1';

const A = Participant('A', { x : UInt });

export const p1 = Reach.App(() => {

  const B = Participant('B', {});
  deploy();

  A.only(() => {
    const x = declassify(interact.x);
    assume(x > 0);
  });
  A.publish(x).pay(x);

  commit();

  B.publish();
  transfer(x).to(B);

  commit();
});
