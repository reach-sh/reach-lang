'reach 0.1';

export const main =
  Reach.App(
    {},
    [Participant('Alice', { request: UInt,
                 info: Bytes(128) }),
     Participant('Bob', { want: Fun([UInt], Null),
                 got: Fun([Bytes(128)], Null) })],
    (A, B) => {
      A.only(() => {
        const request = declassify(interact.request); });
      A.publish(request);
      commit();

      B.only(() => {
        interact.want(request); });
      B.pay(request);
      commit();

      A.only(() => {
        const info = declassify(interact.info); });
      A.publish(info);
      transfer(request-1).to(A); // <--- Oops!
      commit();

      B.only(() => {
        interact.got(info); });
      exit();
    } );
