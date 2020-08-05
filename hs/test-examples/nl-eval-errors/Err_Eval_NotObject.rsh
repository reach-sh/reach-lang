'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    const obj = 0;
    return obj.y;
  }
);
