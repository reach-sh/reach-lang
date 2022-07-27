'reach 0.1';

export const main = Reach.App(() => {
  const V1 = View({x : UInt}, {x: ["y"]});
  const V2 = View({a : UInt}, {a: ["y"]});
  init();
});
