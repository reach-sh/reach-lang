'reach 0.1';

export const main =
  Reach.App(
    {},
    [['Alice', { request: UInt256,
                 info: Bytes }],
     [  'Bob', { want: Fun([UInt256], Null),
                 got: Fun([Bytes], Null) }]],
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
