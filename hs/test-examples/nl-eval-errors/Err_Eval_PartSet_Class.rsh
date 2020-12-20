'reach 0.1';

export const main =
  Reach.App(
    {},
    [['A', {
      getAddr: Fun([], Address),
      }],
     ['class', 'C', {}] ],
    (A, C) => {
      A.only(() => {
        const addr = declassify(interact.getAddr()); });
      A.publish(addr);
      C.set(addr);
      commit();
      exit();
    });
