'reach 0.1';
const MUInt = Maybe(UInt);
export const main = Reach.App(() => {
  const C = Participant('C', { tok: Token, y: MUInt });
  init();
  C.only(() => { const tok = declassify(interact.tok); });
  C.publish(tok);
  commit();
  C.only(() => {
    const y = declassify(interact.y);
  });
  C.publish(y).pay(((yy) =>
    yy.match({
      Some: (z) => {
        return [0, [z, tok]];
      },
      None: () => {
        return [0];
      }
    })
  )(y));
  commit();
});
