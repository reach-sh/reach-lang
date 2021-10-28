'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('Admin', {
  });
  const U = API('Writer', {
    f: Fun([], Null),
  });
  deploy();
  A.publish();
  commit();
  exit();
});
