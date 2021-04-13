'reach 0.1';
'use strict';

export const main =
  Reach.App(
    {},
    [
      Participant('A', { n: UInt, payment: UInt }),
      Participant('B', {})
    ],
    (A, B) => {
      A.only(() => {
        const payment = declassify(interact.payment);
        const n = declassify(interact.n);
      })
      A.publish(n, payment).pay(payment);
      commit();

      B.publish().pay(payment);

      if (n == 0) {
        transfer(payment * 2).to(A);
      } else if (n == 1) {
        transfer(payment * 2).to(B);
      } else {
        transfer(payment).to(A);
        transfer(payment).to(B);
      }
      commit();
      exit();
    });
