'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {x : UInt});
  const V2 = View({a : UInt}, {a: ["A"]});
  init();
});
