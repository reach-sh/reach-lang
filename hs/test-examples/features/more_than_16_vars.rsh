'reach 0.1';

const Alphabet = Array(UInt256, 26);

const AInter = {
  getAlphabet: Fun([], Alphabet),
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
  }
);
