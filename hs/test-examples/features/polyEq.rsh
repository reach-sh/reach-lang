'reach 0.1';

const aliceInterface = {
  a: Array(UInt, 2),
  ad: Address,
  b: Bool,
  i: UInt,
  m: Maybe(UInt),
  o: Object({ a: UInt, b: Object({ c: Bool })}),
  printBool: Fun([Bool], Null),
  s: Bytes(5),
  t: Tuple(UInt, Bool, Bytes(5)),
};

export const main =
  Reach.App(
    {},
    [Participant('A', aliceInterface)],
    (A) => {
      A.publish();
      commit();
      A.only(() => {
        // Statics
        interact.printBool(1 == 1);
        interact.printBool(1 == 12);
        interact.printBool(true == true);
        interact.printBool(true == false);
        interact.printBool('reach' == 'reach');
        interact.printBool('reach' == 'javascript');
        interact.printBool(array(UInt, [1, 2]) == array(UInt, [1, 2]));
        interact.printBool(array(UInt, [1, 2]) == array(UInt, [3, 4]));
        interact.printBool([1, 'r', true] == [1, 'r', true]);
        interact.printBool([1, 'r', true] == [2, 'r', false]);
        interact.printBool({ x: 5, y: true } == { y: true, x: 5 });
        interact.printBool({ x: 5, y: true } == { x: 5, y: false });
        interact.printBool(Maybe(UInt).Some(5) == Maybe(UInt).Some(5));
        interact.printBool(Maybe(UInt).Some(5) == Maybe(UInt).Some(15));
        interact.printBool(Maybe(UInt).Some(5) == Maybe(UInt).None());
        // Dynamics
        interact.printBool(1 == interact.i);
        interact.printBool(true == interact.b);
        interact.printBool('reach' == interact.s);
        interact.printBool(array(UInt, [1, 2]) == interact.a);
        interact.printBool(array(UInt, [interact.i, 2]) == interact.a);
        interact.printBool([interact.i, 'r', interact.b] == interact.t);
        interact.printBool({ a: 5, b: {c: false } } == interact.o);
        interact.printBool(Maybe(UInt).Some(5) == interact.m);
        interact.printBool(A == interact.ad);
      });
    }
  );
