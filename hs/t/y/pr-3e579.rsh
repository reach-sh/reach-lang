'reach 0.1';

export const main = Reach.App(() => {
  setOptions({ connectors: [ALGO] });
  const Alice = Participant('Alice', {
    noticeAlice: Fun([], Null),
  });
  const Bob = Participant('Bob', {
    noticeBob: Fun([], Null),
  });
  deploy();
  const arr = array(UInt, [0, 1, 2, 3, 4]);

  Anybody.publish();

  const i = 8;
  const temp = i < 5 ? arr[i] : i;

  commit();
  exit();
});

// --------------------------------

/*
❯ ./reach compile
Verifying knowledge assertions
Verifying for generic connector
  Verifying when ALL participants are honest
  Verifying when NO participants are honest
  Verifying when ONLY "Alice" is honest
  Verifying when ONLY "Bob" is honest
Checked 10 theorems; No failures!
*/
