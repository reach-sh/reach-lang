'reach 0.1';

const Common = {
  getAddr: Fun([], Address),
  showResult: Fun([Bool], Null),
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
      Alice.publish();
      let s = new Set();
      s.add(Alice);
      commit();

      Bob.only(() => {
        assume(Alice != Bob); });
      Bob.publish();
      require(Alice != Bob);
      s.add(Bob);
      commit();

      Alice.only(() => {
        const aa = declassify(interact.getAddr()); });
      Alice.publish(aa);
      const mx = s[aa];
      commit();

      each([Alice, Bob], () => {
        interact.showResult(mx); });
    });
