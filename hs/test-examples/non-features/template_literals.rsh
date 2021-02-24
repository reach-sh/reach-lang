'reach 0.1';
export const main =
  Reach.App(
    {},
    [Participant('A', {})],
    (A) => {
      A.only(() => {
        const day = 'TUESDAY';
        const template = `I will gladly pay you ${DAY} for a hamburger today`;
      });
      exit();
    }
  );
