'reach 0.1';

const obj = {
  id: ((x) => x),
  inc: ((x) => x + 1),
};

export const main = Reach.App(
  {},
  [["A", {
    arg: UInt,
    showRes: Fun([UInt], Null),
  }]],
  (A) => {
    A.only(() => {
      const arg = interact.arg;
      const ks = array(Bytes(3), ['id', 'inc']);
      const xs = Array.map(ks, (k) => obj[k](arg));
      // ^ reachc: error: ./obj_of_fn.rsh:18:42:array ref:
      // Invalid array index. Expected uint256, got: bytes[3]
      const res = Array.sum(xs);
      interact.showRes(res);
    });
  };
);
