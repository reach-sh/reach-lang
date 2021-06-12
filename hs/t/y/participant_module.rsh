'reach 0.1';

const makePart = () => {
  const x = Participant('A', { x: UInt });
  assert( 4 > 3 );
  return x;
}

export const main =
  Reach.App(() => {
    const A = makePart();
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
