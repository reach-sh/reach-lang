'reach ${REACH_VERSION_SHORT}';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    // Specify Alice's interact interface here
  });
  const Bob   = Participant('Bob', {
    // Specify Bob's interact interface here
  });
  deploy();
  // write your program here
});
