'reach 0.1';

/*
  Allow a Reach program to consume non-network tokens
*/

export const main =
  Reach.App(
    {},
    [Participant('A', {}), Token, Array(Token, 2)],
    (A, token, tokens) => {
      A.pay(5).currency(token);
      transfer(5).currency(token).to(A);
      exit(); }
  );
