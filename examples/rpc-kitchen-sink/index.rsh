'reach 0.1';

import * as runView from './runView.rsh';

export const five = 5;
export const a    = { b: { c: 'd', e: [ five, 6 ] }};

export const timesTwoPlusThree = is((x => x * 2 + 3), Fun([ UInt ], UInt));

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    ...runView.pii,
  });

  const [ v1, v2 ] = [ View('Main', runView.T), View(runView.T) ];

  setOptions({ verifyArithmetic: true, verifyPerConnector: true });
  deploy(); A.publish(); commit();

  // Temporarily disable until ETH gas estimation + arbitrary view-checking order are fixed
  // runView.vbytes(A, v1, Bytes(10).pad('Main/who'), Bytes(10).pad('Main/meta'));
  runView.vbytes(A, v2, Bytes(10).pad('who'),      Bytes(10).pad('meta'));

  runView.fun(A, v1, Bytes(10).pad('Main/fun'));
  runView.fun(A, v2, Bytes(10).pad('fun'));
  A.publish(); commit();

  exit();
});
