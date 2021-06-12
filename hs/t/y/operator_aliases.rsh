'reach 0.1';

const app21 = (f) => {
  return f(2, 1);
};

export const main = Reach.App(
  {}, [], () => {
    assert(app21(add) == 3);
    assert(app21(sub) == 1);
    assert(app21(mul) == 2);
    assert(app21(div) == 2);
    assert(app21(mod) == 0);
    assert(!app21(lt));
    assert(!app21(le));
    assert(!app21(eq));
    assert(app21(ge));
    assert(app21(gt));
  }
);
