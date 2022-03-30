'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', { a: Address, b: Address });
  init();
  A.only(() => {
    const a = declassify(interact.a);
    const b = declassify(interact.b);
    const c = a % b;
  })
});
