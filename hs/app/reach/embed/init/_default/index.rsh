'reach ${REACH_VERSION_SHORT}';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    // Specify Alice's interact interface here
  });
  const B = Participant('Bob', {
    // Specify Bob's interact interface here
  });
  deploy();
  A.publish(); // The first one to publish deploys the contract
  commit();
  // write your program here
});
