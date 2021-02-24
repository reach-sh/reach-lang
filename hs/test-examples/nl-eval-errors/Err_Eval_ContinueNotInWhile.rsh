'reach 0.1';

export const main = Reach.App(
  {}, [Participant('A', {})], (A) => {
    A.publish();

    // This should trigger Err_Eval_ContinueNotInWhile (?)
    [ x ] = [ 3 ];
    continue;
  }
);
