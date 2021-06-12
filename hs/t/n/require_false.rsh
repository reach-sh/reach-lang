'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.publish();
    require(false);
    require(false); // Multiple failures display
    commit();
  }
);
