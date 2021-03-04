'reach 0.1';

export const main =
  Reach.App(
    {},
    [
      Participant('A', { deadline: UInt }),
      ParticipantClass('B', {})
    ],
    (A, B) => {
      A.publish();
      var x = [ 0, false ];
      invariant(balance() == x[0]);
      while ( true ) {
        commit();
        A.pay(1);
        x = [ x[0] + 2, x[1] ];
        continue;
      }
      commit();
      exit();
    });
