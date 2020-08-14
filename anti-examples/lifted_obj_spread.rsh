'reach 0.1';

export const main = Reach.App(
  {},
  [["A", {getObj: Fun([], Object({"x": UInt256}))}]],
  (A) => {
    A.only(() => {
      const obj = interact.getObj();
      const obj2 = {...obj};
      //           ^ 
      // 9:20:obj: Invalid object spread.
      // Expected object, got: object
      //
      // TODO: make this message better,
      // or somehow lift the restriction
      assert(obj2.x == obj.x);
    });
  }
);
