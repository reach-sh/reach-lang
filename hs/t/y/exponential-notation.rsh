'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { ...hasConsoleLogger });
  init();
  // assert(1e4 === 10_000);
  // assert(52E9 === 52_000_000_000);
  // assert(3e-3 === 0.003);
  // assert(52E-5 === 0.00052);
  // assert(-5e8 === -500_000_000);
  // assert(-23E-4 === -0.0023);
  // assert(1.e2 === 100);
  // assert(1.1e2 === 110.0);
  assert(1.22e1 === 12.2);
});
