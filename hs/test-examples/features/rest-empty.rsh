'reach 0.1';
'use strict';

const f = (x, y, ...args) => {
  return (x + y + args.length); };

assert(f(1,2) == 3);
assert(f(1,2,3) == 4);
assert(f(1,2,3,4) == 5);
