'reach 0.1';

const Alphabet = Array(UInt, 26);

const AInter = {
  getAlphabet: Fun([], Alphabet),
  getLetter: Fun([], UInt),
};


export const main = Reach.App(
  {},
  [['A', AInter]],
  (A) => {
    A.only(() => {
      const [
        a, b, c, d, e,
        f, g, h, i, j,
        k, l, m, n, o,
        p, q, r, s, t,
        u, v, w, x, y,
        z,
      ] = declassify(interact.getAlphabet());
    });
    A.publish(
      a, b, c, d, e,
      f, g, h, i, j,
      k, l, m, n, o,
      p, q, r, s, t,
      u, v, w, x, y,
      z
    );
    const alpha = array(UInt, [
      a, b, c, d, e,
      f, g, h, i, j,
      k, l, m, n, o,
      p, q, r, s, t,
      u, v, w, x, y,
      z,
    ]);
    commit();

    A.only(() => {
      const li = declassify(interact.getLetter());
      assume(li < 26);
      const lx = alpha[li]; });
    A.publish(li, lx);
    require(li < 26);
    require(lx == alpha[li]);
    commit();
  }
);
