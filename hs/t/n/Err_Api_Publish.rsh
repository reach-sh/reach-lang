'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {});
  const B = API({ b: Fun([], Null) });
  deploy();

  A.publish();
  commit();

  B.b.publish()
  commit();
});
