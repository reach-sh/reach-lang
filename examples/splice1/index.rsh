'reach 0.1';
export const main = Reach.App(() => {
  const A = Participant('A', {});
  setOptions({untrustworthyMaps: true});
  init();
  A.publish();
  const M1 = new Map(Bytes(128));
  const M2 = new Map(Null);
  M2[A] = null;
  commit();
  A.publish();
  delete M2[A];
  commit();
  exit();
});
