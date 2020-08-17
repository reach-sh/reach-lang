'reach 0.1';

export const main = Reach.App(
  {},
  [["A", {getKey: Fun([], Bytes)}]],
  (A) => {
    A.only(() => {
      const key = interact.getKey();
      assume(key == "x");

      const obj = {[key]: 1}
      //           ^ 
      // 11:20:computed field name: Invalid computed field name.
      // It must be computable at compile time.
      // 
      // TODO: lift the restriction by inspecting assumptions.
      require(obj.x == 1);
    });
  }
);
