'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { ...hasConsoleLogger });
  init();
  // pos coefficient, pos exponent, lower `e`
  assert(1e4 === 10_000);
  // neg coefficient, pos exponent, upper `e`
  assert(-5E8 === -500_000_000);
  // neg coefficient, neg exponent
  assert(-23E-4 === -0.0023);
  // coefficient specifies 0 decimal precision
  assert(1.e2 === 100);
  // coefficient specifies 1 decimal precision
  assert(1.1e2 === 110.0);
  // coefficient specifies 2 decimal precision
  assert(1.22e1 === 12.20);
  // neg exponent specifies 3 decimal precision
  assert(3e-3 === 0.003);
  // coefficient precision > exponent precision
  assert(12.3456e-2 === .1234);
  // coefficient precision < exponent precision
  assert(12.34e-3 === .012);


  // pos coefficient, pos exponent, upper `e`
  assert(52E9 === 52_000_000_000);
  // neg coefficient, pos exponent, lower `e`
  assert(-5e8 === -500_000_000);
  // neg exponent specifies 5 decimal precision
  assert(52E-5 === 0.00052);
  // neg coefficient with precision < exponent precision
  assert(-12.34e-3 === -.012);
  // coefficient's 2 decimal precision overrides negative exponent's precision
  assert(12.34e-1 === 1.23);
});
