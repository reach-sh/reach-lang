'reach 0.1';

const Common = {
  getX: Fun([], UInt),
  getAddr: Fun([], Address),
  showResult: Fun([Maybe(UInt)], Null),
};

export const main =
  Reach.App(
    { 'deployMode': 'firstMsg' },
    [['Alice',
      { ...Common } ],
     ['Bob',
      { ...Common } ],
    ],
    (Alice, Bob) => {
      Alice.only(() => {
        const ax = declassify(interact.getX()); });
      Alice.publish(ax);
      let m = new Map(UInt);
      m[Alice] = ax;
      commit();

      Bob.only(() => {
        const bx = declassify(interact.getX());
        assume(Alice != Bob); });
      Bob.publish(bx);
      require(Alice != Bob);
      m[Bob] = bx;
      commit();

      Alice.only(() => {
        const aa = declassify(interact.getAddr()); });
      Alice.publish(aa);
      const mx = m[aa];
      commit();

      each([Alice, Bob], () => {
        interact.showResult(mx); });
    });
