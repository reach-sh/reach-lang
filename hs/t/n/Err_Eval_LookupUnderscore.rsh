'reach 0.1';

export const main = Reach.App(
  {}, [], () => {
    const [x, _] = [1, "ignored"];
    const y = _;
  }
);
