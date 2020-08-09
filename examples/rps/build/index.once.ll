#lang ll
parts {
  "A" = interact {
    commits = T_Fun [] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Array [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    reveals = T_Fun [T_Bytes] T_Null},
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    shows = T_Fun [] T_Null},
  "O" = interact {
    }};

only("A") {
  const "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2 = interact("A")."getParams"();
  const "array idx":T_UInt256:3 = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 0];
  const "array idx":T_UInt256:4 = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 1];
   };
only("A") {
  const "prim":T_UInt256:8 = CP ADD("array idx":T_UInt256:3,"array idx":T_UInt256:4);
   };
publish("A", join("A":T_Address:7))("array idx":T_UInt256:3,"array idx":T_UInt256:4)("array idx":T_UInt256:5, "array idx":T_UInt256:6).pay("prim":T_UInt256:8){
  const "prim":T_UInt256:9 = CP ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
  const "prim":T_UInt256:10 = CP TXN_VALUE();
  const "prim":T_Bool:11 = CP PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
  claim(CT_Require)("prim":T_Bool:11);
  commit();
  only("B") {
    const "interact":T_Null:13 = interact("B")."partnerIs"("A":T_Address:7);
    const "interact":T_Null:14 = interact("B")."acceptParams"("array idx":T_UInt256:5,"array idx":T_UInt256:6);
     };
  claim(CT_Require)(DLC_Bool True);
  only("B") {
     };
  publish("B", join("B":T_Address:15))()().pay("array idx":T_UInt256:5).timeout((DLC_Int 10, {
    only("A") {
       };
    publish("A", again("A":T_Address:7))()().pay(DLC_Int 0){
      const "prim":T_UInt256:23 = CP TXN_VALUE();
      const "prim":T_Bool:24 = CP PEQ(DLC_Int 0,"prim":T_UInt256:23);
      claim(CT_Require)("prim":T_Bool:24);
      const "prim":T_UInt256:25 = CP BALANCE();
      transfer.("prim":T_UInt256:25).to("A":T_Address:7);
      commit();
      exit(DLC_Bytes "Bob quits"); } })){
    const "prim":T_UInt256:16 = CP TXN_VALUE();
    const "prim":T_Bool:17 = CP PEQ("array idx":T_UInt256:5,"prim":T_UInt256:16);
    claim(CT_Require)("prim":T_Bool:17);
    commit();
    only("A") {
      const "interact":T_Null:27 = interact("A")."partnerIs"("B":T_Address:15);
      let "_handA (as clo app)":T_UInt256:29;
      const "s (as interact)":T_Bytes:30 = interact("A")."getHand"();
      const "rockP (as prim)":T_Bool:31 = CP BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "ROCK");
      const "paperP (as prim)":T_Bool:32 = CP BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "PAPER");
      const "scissorsP (as prim)":T_Bool:33 = CP BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "SCISSORS");
      const "_handA (as prim)":T_Bool:35 = CP IF_THEN_ELSE("rockP (as prim)":T_Bool:31,DLC_Bool True,"paperP (as prim)":T_Bool:32);
      const "_handA (as prim)":T_Bool:37 = CP IF_THEN_ELSE("_handA (as prim)":T_Bool:35,DLC_Bool True,"scissorsP (as prim)":T_Bool:33);
      claim(CT_Assume)("_handA (as prim)":T_Bool:37);
      if "rockP (as prim)":T_Bool:31 then {
        "_handA (as clo app)":T_UInt256:29 = DLC_Int 0;
         }
      else {
        if "paperP (as prim)":T_Bool:32 then {
          "_handA (as clo app)":T_UInt256:29 = DLC_Int 1;
           }
        else {
          "_handA (as clo app)":T_UInt256:29 = DLC_Int 2;
           };
         };
      const "_handA (as prim)":T_Bool:40 = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:29);
      const "_handA (as prim)":T_Bool:41 = CP PLT("_handA (as clo app)":T_UInt256:29,DLC_Int 3);
      const "_handA (as prim)":T_Bool:43 = CP IF_THEN_ELSE("_handA (as prim)":T_Bool:40,"_handA (as prim)":T_Bool:41,DLC_Bool False);
      claim(CT_Assert)("_handA (as prim)":T_Bool:43);
      const "salt (as prim)":T_UInt256:45 = RANDOM();
      const "commitment (as digest)":T_UInt256:46 = digest("salt (as prim)":T_UInt256:45,"_handA (as clo app)":T_UInt256:29);
      const "interact":T_Null:47 = interact("A")."commits"();
       };
    claim(CT_Require)(DLC_Bool True);
    only("A") {
       };
    publish("A", again("A":T_Address:7))("commitment (as digest)":T_UInt256:46)("commitment (as digest)":T_UInt256:48).pay(DLC_Int 0).timeout((DLC_Int 10, {
      only("B") {
         };
      publish("B", again("B":T_Address:15))()().pay(DLC_Int 0){
        const "prim":T_UInt256:56 = CP TXN_VALUE();
        const "prim":T_Bool:57 = CP PEQ(DLC_Int 0,"prim":T_UInt256:56);
        claim(CT_Require)("prim":T_Bool:57);
        const "prim":T_UInt256:58 = CP BALANCE();
        transfer.("prim":T_UInt256:58).to("B":T_Address:15);
        commit();
        exit(DLC_Bytes "Alice quits"); } })){
      const "prim":T_UInt256:49 = CP TXN_VALUE();
      const "prim":T_Bool:50 = CP PEQ(DLC_Int 0,"prim":T_UInt256:49);
      claim(CT_Require)("prim":T_Bool:50);
      commit();
      only("B") {
        let "handB (as clo app)":T_UInt256:61;
        const "s (as interact)":T_Bytes:62 = interact("B")."getHand"();
        const "rockP (as prim)":T_Bool:63 = CP BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:64 = CP BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:65 = CP BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "SCISSORS");
        const "handB (as prim)":T_Bool:67 = CP IF_THEN_ELSE("rockP (as prim)":T_Bool:63,DLC_Bool True,"paperP (as prim)":T_Bool:64);
        const "handB (as prim)":T_Bool:69 = CP IF_THEN_ELSE("handB (as prim)":T_Bool:67,DLC_Bool True,"scissorsP (as prim)":T_Bool:65);
        claim(CT_Assume)("handB (as prim)":T_Bool:69);
        if "rockP (as prim)":T_Bool:63 then {
          "handB (as clo app)":T_UInt256:61 = DLC_Int 0;
           }
        else {
          if "paperP (as prim)":T_Bool:64 then {
            "handB (as clo app)":T_UInt256:61 = DLC_Int 1;
             }
          else {
            "handB (as clo app)":T_UInt256:61 = DLC_Int 2;
             };
           };
        const "handB (as prim)":T_Bool:72 = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:61);
        const "handB (as prim)":T_Bool:73 = CP PLT("handB (as clo app)":T_UInt256:61,DLC_Int 3);
        const "handB (as prim)":T_Bool:75 = CP IF_THEN_ELSE("handB (as prim)":T_Bool:72,"handB (as prim)":T_Bool:73,DLC_Bool False);
        claim(CT_Assert)("handB (as prim)":T_Bool:75);
        const "interact":T_Null:76 = interact("B")."shows"();
         };
      claim(CT_Require)(DLC_Bool True);
      only("B") {
         };
      publish("B", again("B":T_Address:15))("handB (as clo app)":T_UInt256:61)("handB (as clo app)":T_UInt256:77).pay(DLC_Int 0).timeout((DLC_Int 10, {
        only("A") {
           };
        publish("A", again("A":T_Address:7))()().pay(DLC_Int 0){
          const "prim":T_UInt256:85 = CP TXN_VALUE();
          const "prim":T_Bool:86 = CP PEQ(DLC_Int 0,"prim":T_UInt256:85);
          claim(CT_Require)("prim":T_Bool:86);
          const "prim":T_UInt256:87 = CP BALANCE();
          transfer.("prim":T_UInt256:87).to("A":T_Address:7);
          commit();
          exit(DLC_Bytes "Bob quits"); } })){
        const "prim":T_UInt256:78 = CP TXN_VALUE();
        const "prim":T_Bool:79 = CP PEQ(DLC_Int 0,"prim":T_UInt256:78);
        claim(CT_Require)("prim":T_Bool:79);
        const "prim":T_Bool:89 = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
        const "prim":T_Bool:90 = CP PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
        const "prim":T_Bool:92 = CP IF_THEN_ELSE("prim":T_Bool:89,"prim":T_Bool:90,DLC_Bool False);
        claim(CT_Require)("prim":T_Bool:92);
        commit();
        only("A") {
          let "clo app":T_Bytes:94;
          const "prim":T_Bool:96 = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:97 = CP PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:99 = CP IF_THEN_ELSE("prim":T_Bool:96,"prim":T_Bool:97,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:99);
          const "prim":T_Bool:100 = CP PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 0);
          if "prim":T_Bool:100 then {
            "clo app":T_Bytes:94 = DLC_Bytes "ROCK";
             }
          else {
            const "prim":T_Bool:101 = CP PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 1);
            if "prim":T_Bool:101 then {
              "clo app":T_Bytes:94 = DLC_Bytes "PAPER";
               }
            else {
              "clo app":T_Bytes:94 = DLC_Bytes "SCISSORS";
               };
             };
          const "interact":T_Null:102 = interact("A")."reveals"("clo app":T_Bytes:94);
           };
        claim(CT_Require)(DLC_Bool True);
        only("A") {
           };
        publish("A", again("A":T_Address:7))("salt (as prim)":T_UInt256:45,"_handA (as clo app)":T_UInt256:29)("salt (as prim)":T_UInt256:103, "_handA (as clo app)":T_UInt256:104).pay(DLC_Int 0).timeout((DLC_Int 10, {
          only("B") {
             };
          publish("B", again("B":T_Address:15))()().pay(DLC_Int 0){
            const "prim":T_UInt256:112 = CP TXN_VALUE();
            const "prim":T_Bool:113 = CP PEQ(DLC_Int 0,"prim":T_UInt256:112);
            claim(CT_Require)("prim":T_Bool:113);
            const "prim":T_UInt256:114 = CP BALANCE();
            transfer.("prim":T_UInt256:114).to("B":T_Address:15);
            commit();
            exit(DLC_Bytes "Alice quits"); } })){
          const "prim":T_UInt256:105 = CP TXN_VALUE();
          const "prim":T_Bool:106 = CP PEQ(DLC_Int 0,"prim":T_UInt256:105);
          claim(CT_Require)("prim":T_Bool:106);
          const "digest":T_UInt256:116 = digest("salt (as prim)":T_UInt256:103,"_handA (as clo app)":T_UInt256:104);
          const "prim":T_Bool:117 = CP PEQ("commitment (as digest)":T_UInt256:48,"digest":T_UInt256:116);
          claim(CT_Require)("prim":T_Bool:117);
          const "prim":T_Bool:119 = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
          const "prim":T_Bool:120 = CP PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
          const "prim":T_Bool:122 = CP IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:122);
          let "outcome (as clo app)":T_UInt256:124;
          const "validA (as prim)":T_Bool:126 = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
          const "validA (as prim)":T_Bool:127 = CP PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
          const "validA (as prim)":T_Bool:129 = CP IF_THEN_ELSE("validA (as prim)":T_Bool:126,"validA (as prim)":T_Bool:127,DLC_Bool False);
          const "validB (as prim)":T_Bool:131 = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "validB (as prim)":T_Bool:132 = CP PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "validB (as prim)":T_Bool:134 = CP IF_THEN_ELSE("validB (as prim)":T_Bool:131,"validB (as prim)":T_Bool:132,DLC_Bool False);
          const "outcome (as prim)":T_Bool:136 = CP IF_THEN_ELSE("validA (as prim)":T_Bool:129,"validB (as prim)":T_Bool:134,DLC_Bool False);
          if "outcome (as prim)":T_Bool:136 then {
            const "outcome (as prim)":T_UInt256:137 = CP SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:77);
            const "outcome (as prim)":T_UInt256:138 = CP ADD("_handA (as clo app)":T_UInt256:104,"outcome (as prim)":T_UInt256:137);
            const "outcome (as prim)":T_UInt256:139 = CP MOD("outcome (as prim)":T_UInt256:138,DLC_Int 3);
            "outcome (as clo app)":T_UInt256:124 = "outcome (as prim)":T_UInt256:139;
             }
          else {
            if "validA (as prim)":T_Bool:129 then {
              "outcome (as clo app)":T_UInt256:124 = DLC_Int 2;
               }
            else {
              if "validB (as prim)":T_Bool:134 then {
                "outcome (as clo app)":T_UInt256:124 = DLC_Int 0;
                 }
              else {
                "outcome (as clo app)":T_UInt256:124 = DLC_Int 1;
                 };
               };
             };
          const "outcome (as prim)":T_Bool:142 = CP PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
          const "outcome (as prim)":T_Bool:143 = CP PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
          const "outcome (as prim)":T_Bool:145 = CP IF_THEN_ELSE("outcome (as prim)":T_Bool:142,"outcome (as prim)":T_Bool:143,DLC_Bool False);
          claim(CT_Assert)("outcome (as prim)":T_Bool:145);
          const "prim":T_Bool:146 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
          const "prim":T_Bool:148 = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
          const "prim":T_Bool:149 = CP PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
          const "prim":T_Bool:151 = CP IF_THEN_ELSE("prim":T_Bool:148,"prim":T_Bool:149,DLC_Bool False);
          const "prim":T_Bool:154 = CP IF_THEN_ELSE("prim":T_Bool:146,DLC_Bool False,DLC_Bool True);
          const "prim":T_Bool:156 = CP IF_THEN_ELSE("prim":T_Bool:154,DLC_Bool True,"prim":T_Bool:151);
          claim(CT_Assert)("prim":T_Bool:156);
          const "prim":T_Bool:157 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
          const "prim":T_Bool:159 = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:160 = CP PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:162 = CP IF_THEN_ELSE("prim":T_Bool:159,"prim":T_Bool:160,DLC_Bool False);
          const "prim":T_Bool:165 = CP IF_THEN_ELSE("prim":T_Bool:157,DLC_Bool False,DLC_Bool True);
          const "prim":T_Bool:167 = CP IF_THEN_ELSE("prim":T_Bool:165,DLC_Bool True,"prim":T_Bool:162);
          claim(CT_Assert)("prim":T_Bool:167);
          const "prim":T_Bool:169 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
          const "prim":T_Bool:172 = CP PEQ("_handA (as clo app)":T_UInt256:104,DLC_Int 0);
          const "prim":T_Bool:174 = CP IF_THEN_ELSE("prim":T_Bool:172,"prim":T_Bool:169,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:174);
          const "prim":T_Bool:176 = CP PEQ("_handA (as clo app)":T_UInt256:104,DLC_Int 1);
          const "prim":T_Bool:178 = CP IF_THEN_ELSE("prim":T_Bool:176,"prim":T_Bool:169,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:178);
          const "prim":T_Bool:180 = CP PEQ("_handA (as clo app)":T_UInt256:104,DLC_Int 2);
          const "prim":T_Bool:182 = CP IF_THEN_ELSE("prim":T_Bool:180,"prim":T_Bool:169,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:182);
          const "prim":T_Bool:183 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
          const "prim":T_Bool:186 = CP PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 0);
          const "prim":T_Bool:188 = CP IF_THEN_ELSE("prim":T_Bool:186,"prim":T_Bool:183,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:188);
          const "prim":T_Bool:190 = CP PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 1);
          const "prim":T_Bool:192 = CP IF_THEN_ELSE("prim":T_Bool:190,"prim":T_Bool:183,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:192);
          const "prim":T_Bool:194 = CP PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 2);
          const "prim":T_Bool:196 = CP IF_THEN_ELSE("prim":T_Bool:194,"prim":T_Bool:183,DLC_Bool False);
          claim(CT_Possible)("prim":T_Bool:196);
          const "prim":T_Bool:197 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
          if "prim":T_Bool:197 then {
            transfer.(DLC_Int 0).to("A":T_Address:7);
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:199 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:199 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:200 = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:200,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:201 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:201 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:202 = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:202];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
                 };
               };
            const "array idx":T_UInt256:203 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198[DLC_Int 0];
            const "array idx":T_UInt256:204 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198[DLC_Int 1];
            const "prim":T_UInt256:205 = CP ADD("array idx":T_UInt256:6,"array idx":T_UInt256:203);
            transfer.("prim":T_UInt256:205).to("A":T_Address:7);
            transfer.("array idx":T_UInt256:204).to("B":T_Address:15);
            commit();
            let "clo app":T_Bytes:206;
            const "prim":T_Bool:208 = CP PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
            const "prim":T_Bool:209 = CP PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
            const "prim":T_Bool:211 = CP IF_THEN_ELSE("prim":T_Bool:208,"prim":T_Bool:209,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:211);
            const "prim":T_Bool:212 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
            if "prim":T_Bool:212 then {
              "clo app":T_Bytes:206 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:213 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 1);
              if "prim":T_Bool:213 then {
                "clo app":T_Bytes:206 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:214 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
                if "prim":T_Bool:214 then {
                  "clo app":T_Bytes:206 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:215 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 3);
                  if "prim":T_Bool:215 then {
                    "clo app":T_Bytes:206 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:206 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            exit("clo app":T_Bytes:206); }
          else {
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:199 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:199 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:200 = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:200,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:201 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:201 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:202 = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:202];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
                 };
               };
            const "array idx":T_UInt256:203 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198[DLC_Int 0];
            const "array idx":T_UInt256:204 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:198[DLC_Int 1];
            const "prim":T_UInt256:205 = CP ADD("array idx":T_UInt256:6,"array idx":T_UInt256:203);
            transfer.("prim":T_UInt256:205).to("A":T_Address:7);
            transfer.("array idx":T_UInt256:204).to("B":T_Address:15);
            commit();
            let "clo app":T_Bytes:206;
            const "prim":T_Bool:208 = CP PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
            const "prim":T_Bool:209 = CP PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
            const "prim":T_Bool:211 = CP IF_THEN_ELSE("prim":T_Bool:208,"prim":T_Bool:209,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:211);
            const "prim":T_Bool:212 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
            if "prim":T_Bool:212 then {
              "clo app":T_Bytes:206 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:213 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 1);
              if "prim":T_Bool:213 then {
                "clo app":T_Bytes:206 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:214 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
                if "prim":T_Bool:214 then {
                  "clo app":T_Bytes:206 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:215 = CP PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 3);
                  if "prim":T_Bool:215 then {
                    "clo app":T_Bytes:206 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:206 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            exit("clo app":T_Bytes:206); }; } } } } }