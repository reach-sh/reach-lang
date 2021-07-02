'reach 0.1';

export const main = Reach.App(() => {
  const Alice = Participant('Alice', { y: UInt });
  deploy();
  const x = 0;
  Alice.only(() => {
    const y = declassify(interact.y);
  });
  Alice.publish(y);
  commit();
  const a = x[y];
});
