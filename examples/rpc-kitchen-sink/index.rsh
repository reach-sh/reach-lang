'reach 0.1';

import * as runView           from './runView.rsh';
import { MUInt, MA, MBS, BS } from './type.rsh';

const v = {
  fun:  Fun([UInt], UInt),
  who:  Address,
  meta: BS,
};

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    checkViewFun:   Fun([ Bytes(10), UInt, MUInt ], Null),
    checkViewBytes: Fun([ Bytes(10), Bytes(10), Tuple(MA, MBS) ], Null),
    meta: BS,
  });

  const [ v1, v2 ] = [ View('Main', v), View(v) ];

  deploy(); A.publish(); commit();

  runView.vbytes(A, v1, Bytes(10).pad('Main/who'), Bytes(10).pad('Main/meta'));
  runView.vbytes(A, v2, Bytes(10).pad('who'),      Bytes(10).pad('meta'));

  runView.fun(A, v1, Bytes(10).pad('Main/fun'));
  runView.fun(A, v2, Bytes(10).pad('fun'));

  exit();
});
