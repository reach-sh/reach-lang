'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', { verifyNumber: Fun([UInt], Null) });
  init();
  A.publish();
  A.interact.verifyNumber(123456789);
  A.interact.verifyNumber(123_456_789);
    A.only(() => {
    // Starting with an underscore makes an identifier
    const _123 = interact.verifyNumber(1_2_3_4_5_6_7_8_9);
  })
  commit();
});
