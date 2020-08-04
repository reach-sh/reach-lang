'reach 0.1';

export const main = Reach.App(
  {}, [["A", {}]], (A) => {
    A.publish();
    A.only(() => { return 0; });
    commit();
    return 0;
  }
);
