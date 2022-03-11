'reach 0.1';

const nDec = 123_456_789;

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    // Specify Alice's interact interface here
    verifyNumbers: Fun([UInt], Null),
  });
  init();
  A.publish();
  A.interact.verifyNumbers(nDec);
  commit();
});
