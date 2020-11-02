'reach 0.1';

const Y = (f) => {
  const k = (x) => f(x(x));
  return k(k);
};

const fib = Y((fib) => (x) => {
  switch (x) {
    case 0: return 0;
    case 1: return 1;
    default: return fib(x - 1) + fib(x - 2);
  }
});

export const main = Reach.App(
  {}, [['Alice', {x: UInt, tell: Fun([UInt], Null)}]], (Alice) => {
    Alice.only(() => {
      interact.tell(fib(interact.x));
    });
  }
);
