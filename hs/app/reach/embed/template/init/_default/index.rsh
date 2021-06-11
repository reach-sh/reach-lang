'reach ${REACH_VERSION_SHORT}';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  const Bob   = Participant('Bob', {});
  deploy();
  // write your program here

});
