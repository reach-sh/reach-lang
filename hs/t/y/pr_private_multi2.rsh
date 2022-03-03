'reach 0.1';

export const main = Reach.App(() => {
  const A = Participant('A', {
    when: Fun([], Bool),
    when2: Fun([], Bool),
    when3: Fun([], Bool),
    sec1: UInt,
    sec2: Bool,
    seeP: Fun([UInt,Bool], Null)
  });
  const B = ParticipantClass('B', {
    when: Fun([], Bool)
  });
  const C = ParticipantClass('C', {
    when: Fun([], Bool),
    sec: UInt,
    seeP: Fun([UInt], Null)
  });
  init();

  A.publish();

  const [ alive ] =
    parallelReduce([ true ])
      .invariant(balance() == 0)
      .while(alive)
      .case(A,
        () => {
          const _sec1 = interact.sec1;
          const _sec2 = interact.sec2;
          return {
            when: declassify(interact.when()),
            _local: [_sec1, _sec2]
          };
        },
        (_, [_sec1, _sec2]) => {
          A.only(() => {
            interact.seeP(_sec1, _sec2);
          });
          return [ true ];
      })
      .case(A,
        () => {
          const _sec1 = interact.sec1;
          return {
            when: declassify(interact.when2()),
            _local: _sec1
          };
        },
        (_, _sec1) => {
          A.only(() => {
            interact.seeP(_sec1, false);
          });
          return [ true ];
      })
      .case(A,
        () => {
          return {
            when: declassify(interact.when3()),
            // no local
          };
        },
        (_) => {
          return [ true ];
      })
      .case(B,
        () => {
          return {
            when: declassify(interact.when()),
            // no local
          };
        },
        (_) => {
          return [ true ];
      })
      .case(C,
        () => {
          return {
            when: declassify(interact.when()),
            _local: interact.sec,
          };
        },
        (_, _sec) => {
          C.only(() => {
            interact.seeP(_sec);
          });
          return [ true ];
      })
      .timeout(relativeSecs(25), () => {
        Anybody.publish();
        return [ false ];
      });

  commit();

});
