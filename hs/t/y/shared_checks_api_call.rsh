'reach 0.1';
'use strict';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API('B', {
    getNumbers: Fun([UInt, UInt], UInt),
  })
  init();

  A.publish();
  commit();

  const [ [ x, y ], k ] =
    call(B.getNumbers)
      .check((x, y) => {
        check(x + y < 10, "x + y < 10");
      });

  k(x + y);

  commit();
  assert(x + y < 10, "x + y < 10");

})
