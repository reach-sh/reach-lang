'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Alice', {
    getAddr: Fun([], Address),
  });
  const U = API('User', {
    x: UInt,
  });
  deploy();
  exit();
});
