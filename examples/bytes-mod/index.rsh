'reach 0.1';

const B1CHUNK_LT8 = Bytes(5);
const B1CHUNK_GE8 = Bytes(24);
const B2CHUNK_LT8 = Bytes(36);
const B2CHUNK_GE8 = Bytes(40);

export const main = Reach.App(() => {
  const A = Participant('A', {
    b1: B1CHUNK_LT8,
    b2: B1CHUNK_GE8,
    b3: B2CHUNK_LT8,
    b4: B2CHUNK_GE8,
    expected1: UInt,
    expected2: UInt,
    expected3: UInt,
    expected4: UInt,
  });
  init();
  A.only(() => {
    const b1 = declassify(interact.b1);
    const b2 = declassify(interact.b2);
    const b3 = declassify(interact.b3);
    const b4 = declassify(interact.b4);
    const e1 = declassify(interact.expected1);
    const e2 = declassify(interact.expected2);
    const e3 = declassify(interact.expected3);
    const e4 = declassify(interact.expected4);
    check(e1 == b1 % 4, "B1CHUNK_LT8");
    check(e2 == b2 % 4, "B1CHUNK_GE8");
    check(e3 == b3 % 4, "B2CHUNK_LT8");
    check(e4 == b4 % 4, "B2CHUNK_GE8");
  });
  A.publish(b1, b2, b3, b4, e1, e2, e3, e4);
  check(e1 == b1 % 4, "B1CHUNK_LT8");
  check(e2 == b2 % 4, "B1CHUNK_GE8");
  check(e3 == b3 % 4, "B2CHUNK_LT8");
  check(e4 == b4 % 4, "B2CHUNK_GE8");
  commit();
});
