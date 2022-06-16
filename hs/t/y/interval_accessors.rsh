'reach 0.1';

const testLeftEndpoint = () => {
  const y = intervalCC(+2, +4);
  const x = intervalCC(+8, +12);
  assert(leftEndpoint(y) == +2);
}

const testRightEndpoint = () => {
  const y = intervalCO(+2, +4);
  const x = intervalCO(+8, +12);
  assert(rightEndpoint(x) !== rightEndpoint(y));
}

export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
        testLeftEndpoint();
        testRightEndpoint()
    });
