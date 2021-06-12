'reach 0.1';

const f = (x, y, ...args) => {
  if ( args.length > 0 && args[0] == false ) {
    return 42;
  } else {
    return x + y + args.length; } };

assert(f(1,2) == 3);
assert(f(1,2,3) == 4);
assert(f(1,2,false,4) == 42);
