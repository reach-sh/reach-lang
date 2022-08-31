'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    b4: Bytes(4)
  });
  init();

  A.only(() => {
    const b4 = declassify(interact.b4);
  });
  A.publish(b4).check(() => {
    check(b4 == Bytes.fromHex('0x01ffc9a7'), "is equals");
  });

  commit();
});
