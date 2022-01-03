'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  init();
  const x = array(UInt, [0, 1, 2]);
  x.map(function m(i){ return i + 1; });
});
