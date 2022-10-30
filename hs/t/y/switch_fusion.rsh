'reach 0.1';

/*

Before switch fusion:

local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/103 : UInt/True: {
    only(Left "A") {
      const v104 : Address* = selfAddress("A", False, 35 )();
      const v105 : Bytes(6)* = "left 1";
      protect<Null>("A".interact.log(v105 ));
    };
  }
  case Right as ex/106 : Bool/True: {
    only(Left "A") {
      const v107 : Address* = selfAddress("A", False, 41 )();
      const v108 : Bytes(7)* = "right 1";
      protect<Null>("A".interact.log(v108 ));
    };
  }
}
local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/109 : UInt/True: {
    only(Left "A") {
      const v110 : Address* = selfAddress("A", False, 47 )();
      const v111 : Bytes(6)* = "left 2";
      protect<Null>("A".interact.log(v111 ));
    };
  }
  case Right as ex/112 : Bool/True: {
    only(Left "A") {
      const v113 : Address* = selfAddress("A", False, 53 )();
      const v114 : Bytes(7)* = "right 2";
      protect<Null>("A".interact.log(v114 ));
    };
  }
}
local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/115 : UInt/True: {
    only(Left "A") {
      const v116 : Address* = selfAddress("A", False, 59 )();
      const v117 : Bytes(6)* = "left 3";
      protect<Null>("A".interact.log(v117 ));
    };
  }
  case Right as ex/118 : Bool/True: {
    only(Left "A") {
      const v119 : Address* = selfAddress("A", False, 65 )();
      const v120 : Bytes(7)* = "right 3";
      protect<Null>("A".interact.log(v120 ));
    };
  }
}
only(Left "A") {
  const v121 : Address* = selfAddress("A", False, 69 )();
  let v122 : Null;
  v122 : Null = null;
};
local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/123 : UInt/True: {
    only(Left "A") {
      const v124 : Address* = selfAddress("A", False, 74 )();
      const v125 : Bytes(6)* = "left 4";
      protect<Null>("A".interact.log(v125 ));
    };
  }
  case Right as ex/126 : Bool/True: {
    only(Left "A") {
      const v127 : Address* = selfAddress("A", False, 80 )();
      const v128 : Bytes(7)* = "right 4";
      protect<Null>("A".interact.log(v128 ));
    };
  }
}

After switch fusion (variable defn breaks fusing switch 3 and 4):

local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/103 : UInt/True: {
    only(Left "A") {
      const v104 : Address* = selfAddress("A", False, 35 )();
      const v105 : Bytes(6)* = "left 1";
      protect<Null>("A".interact.log(v105 ));
    };
    only(Left "A") {
      const v110 : Address* = selfAddress("A", False, 47 )();
      const v111 : Bytes(6)* = "left 2";
      protect<Null>("A".interact.log(v111 ));
    };
    only(Left "A") {
      const v116 : Address* = selfAddress("A", False, 59 )();
      const v117 : Bytes(6)* = "left 3";
      protect<Null>("A".interact.log(v117 ));
    };
  }
  case Right as ex/106 : Bool/True: {
    only(Left "A") {
      const v107 : Address* = selfAddress("A", False, 41 )();
      const v108 : Bytes(7)* = "right 1";
      protect<Null>("A".interact.log(v108 ));
    };
    only(Left "A") {
      const v113 : Address* = selfAddress("A", False, 53 )();
      const v114 : Bytes(7)* = "right 2";
      protect<Null>("A".interact.log(v114 ));
    };
    only(Left "A") {
      const v119 : Address* = selfAddress("A", False, 65 )();
      const v120 : Bytes(7)* = "right 3";
      protect<Null>("A".interact.log(v120 ));
    };
  }
}
only(Left "A") {
  const v121 : Address* = selfAddress("A", False, 69 )();
  const v122 : Null* = null;
};
local switch (ex/100 : Data({"Left": UInt, "Right": Bool})) {
  case Left as ex/123 : UInt/True: {
    only(Left "A") {
      const v124 : Address* = selfAddress("A", False, 74 )();
      const v125 : Bytes(6)* = "left 4";
      protect<Null>("A".interact.log(v125 ));
    };
  }
  case Right as ex/126 : Bool/True: {
    only(Left "A") {
      const v127 : Address* = selfAddress("A", False, 80 )();
      const v128 : Bytes(7)* = "right 4";
      protect<Null>("A".interact.log(v128 ));
    };
  }
}

*/

export const main = Reach.App(() => {
  const A = Participant('A', {
    log: Fun(true, Null),
    b: Bool,
    ex: Either(UInt, Bool)
  });
  init();

  A.only(() => {
    const b = declassify(interact.b);
    const ex = declassify(interact.ex);
  });
  A.publish(b, ex);

  switch (ex) {
    case Left: {
      A.interact.log("left 1");
    }
    case Right: {
      A.interact.log("right 1");
    }
  }

  switch (ex) {
    case Left: {
      A.interact.log("left 2");
    }
    case Right: {
      A.interact.log("right 2");
    }
  }

  switch (ex) {
    case Left: {
      A.interact.log("left 3");
    }
    case Right: {
      A.interact.log("right 3");
    }
  }

  A.only(() => {
    const d = declassify(interact.b);
  });

  switch (ex) {
    case Left: {
      A.interact.log("left 4");
    }
    case Right: {
      A.interact.log("right 4");
    }
  }

  commit();
});

