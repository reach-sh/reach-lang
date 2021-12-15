'reach 0.1';

const B  = Bytes(64);
const MB = Maybe(B);
const A  = Array(UInt, 2);
const E  = Either(A, MB);

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {});
  const Logger = Events({
    log: [E],
  });
  deploy();

  Alice.publish();

  Logger.log(E.Left(array(UInt, [3, 1415])));

  Logger.log(E.Left(array(UInt, [4, 2000])));

  Logger.log(E.Right(MB.Some(B.pad("THE FUTURE OF BLOCKCHAIN"))));

  commit();

  Alice.publish();

  Logger.log(E.Right(MB.Some(B.pad("IS IN REACH"))));

  commit();

});
