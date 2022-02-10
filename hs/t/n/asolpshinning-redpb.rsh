'reach 0.1';
export const main = Reach.App(() => {
  const C = Participant('C', { tok: Token, y: UInt, z: Bool });
  init();
  C.only(() => { const tok = declassify(interact.tok); });
  C.publish(tok);
  commit();
  C.only(() => {
    const y = declassify(interact.y);
    const z = declassify(interact.z);
  });
  C.publish(y, z).pay(((zz) => {
    if (zz) {
      return [0, [y, tok]];
    } else {
      return [0];
    }
  })(z));
  commit();
});
