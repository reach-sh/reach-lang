'reach 0.1';
export const main = Reach.App(() => {
  const T = Address;
  const N = 10;
  const D = Participant('D', {
    z: Array(T, N),
  });
  const State = View('State', {
    f: Fun([Address], Maybe(T))
  });
  init();
  D.only(() => {
    const z = declassify(interact.z);
  });
  D.publish(z);
  State.f.set((x) => z.find((u) => u == x));
  commit();
  D.publish();
  commit();
  exit();
});
