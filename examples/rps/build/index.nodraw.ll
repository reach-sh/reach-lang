#lang ll
parts {
  "A" = interact {
    commits = T_Fun [] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Array [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    reveals = T_Fun [T_Bytes] T_Null},
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null},
  "O" = interact {
    }};

only("A") {
  const "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2 = interact("A")."getParams"();
  const "array idx":T_UInt256:3 = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 0];
  const "array idx":T_UInt256:4 = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 1];
   };
only("A") {
  const "prim":T_UInt256:8 = ADD("array idx":T_UInt256:3,"array idx":T_UInt256:4);
   };
publish("A", join("A":T_Address:7))("array idx":T_UInt256:3,"array idx":T_UInt256:4)("array idx":T_UInt256:5, "array idx":T_UInt256:6).pay("prim":T_UInt256:8){
  const "prim":T_UInt256:9 = ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
  const "prim":T_UInt256:10 = TXN_VALUE();
  const "prim":T_Bool:11 = PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
  claim(CT_Require)("prim":T_Bool:11);
  commit();
  only("B") {
    const "interact":T_Null:13 = interact("B")."acceptParams"("array idx":T_UInt256:5,"array idx":T_UInt256:6);
     };
  claim(CT_Require)(DLC_Bool True);
  only("B") {
     };
  publish("B", join("B":T_Address:14))()().pay("array idx":T_UInt256:5).timeout((DLC_Int 10, {
    only("A") {
       };
    publish("A", again("A":T_Address:7))()().pay(DLC_Int 0){
      const "prim":T_UInt256:22 = TXN_VALUE();
      const "prim":T_Bool:23 = PEQ(DLC_Int 0,"prim":T_UInt256:22);
      claim(CT_Require)("prim":T_Bool:23);
      const "prim":T_UInt256:24 = BALANCE();
      transfer.("prim":T_UInt256:24).to("A":T_Address:7);
      commit();
      exit(DLC_Bytes "Bob quits"); } })){
    const "prim":T_UInt256:15 = TXN_VALUE();
    const "prim":T_Bool:16 = PEQ("array idx":T_UInt256:5,"prim":T_UInt256:15);
    claim(CT_Require)("prim":T_Bool:16);
    loopvar {
      "count":T_UInt256:25 = DLC_Int 0,
      "outcome":T_UInt256:26 = DLC_Int 1};
    invariant{
      const "prim":T_UInt256:27 = BALANCE();
      const "prim":T_UInt256:28 = MUL(DLC_Int 2,"array idx":T_UInt256:5);
      const "prim":T_UInt256:29 = ADD("prim":T_UInt256:28,"array idx":T_UInt256:6);
      const "prim":T_Bool:30 = PEQ("prim":T_UInt256:27,"prim":T_UInt256:29);
      const "prim":T_Bool:32 = PLE(DLC_Int 0,"outcome":T_UInt256:26);
      const "prim":T_Bool:33 = PLT("outcome":T_UInt256:26,DLC_Int 5);
      const "prim":T_Bool:35 = IF_THEN_ELSE("prim":T_Bool:32,"prim":T_Bool:33,DLC_Bool False);
      const "prim":T_Bool:37 = IF_THEN_ELSE("prim":T_Bool:30,"prim":T_Bool:35,DLC_Bool False);
      const "prim":T_Bool:39 = PEQ("outcome":T_UInt256:26,DLC_Int 3);
      const "prim":T_Bool:41 = IF_THEN_ELSE("prim":T_Bool:39,DLC_Bool False,DLC_Bool True);
      const "prim":T_Bool:43 = IF_THEN_ELSE("prim":T_Bool:37,"prim":T_Bool:41,DLC_Bool False);
      const "prim":T_Bool:45 = PEQ("outcome":T_UInt256:26,DLC_Int 4);
      const "prim":T_Bool:47 = IF_THEN_ELSE("prim":T_Bool:45,DLC_Bool False,DLC_Bool True);
      const "prim":T_Bool:49 = IF_THEN_ELSE("prim":T_Bool:43,"prim":T_Bool:47,DLC_Bool False);
      
      return "prim":T_Bool:49; }
    while{
      const "prim":T_Bool:50 = PEQ("outcome":T_UInt256:26,DLC_Int 1);
      
      return "prim":T_Bool:50; }
    {
      commit();
      only("A") {
        let "_handA (as clo app)":T_UInt256:53;
        const "s (as interact)":T_Bytes:54 = interact("A")."getHand"();
        const "rockP (as prim)":T_Bool:55 = BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:56 = BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:57 = BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "SCISSORS");
        const "_handA (as prim)":T_Bool:59 = IF_THEN_ELSE("rockP (as prim)":T_Bool:55,DLC_Bool True,"paperP (as prim)":T_Bool:56);
        const "_handA (as prim)":T_Bool:61 = IF_THEN_ELSE("_handA (as prim)":T_Bool:59,DLC_Bool True,"scissorsP (as prim)":T_Bool:57);
        claim(CT_Assume)("_handA (as prim)":T_Bool:61);
        if "rockP (as prim)":T_Bool:55 then {
          "_handA (as clo app)":T_UInt256:53 = DLC_Int 0;
           }
        else {
          if "paperP (as prim)":T_Bool:56 then {
            "_handA (as clo app)":T_UInt256:53 = DLC_Int 1;
             }
          else {
            "_handA (as clo app)":T_UInt256:53 = DLC_Int 2;
             };
           };
        const "_handA (as prim)":T_Bool:64 = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:53);
        const "_handA (as prim)":T_Bool:65 = PLT("_handA (as clo app)":T_UInt256:53,DLC_Int 3);
        const "_handA (as prim)":T_Bool:67 = IF_THEN_ELSE("_handA (as prim)":T_Bool:64,"_handA (as prim)":T_Bool:65,DLC_Bool False);
        claim(CT_Assert)("_handA (as prim)":T_Bool:67);
        const "salt (as interact)":T_UInt256:69 = interact("A")."random"();
        const "commitment (as digest)":T_UInt256:70 = digest("salt (as interact)":T_UInt256:69,"_handA (as clo app)":T_UInt256:53);
        const "interact":T_Null:71 = interact("A")."commits"();
         };
      claim(CT_Require)(DLC_Bool True);
      only("A") {
         };
      publish("A", again("A":T_Address:7))("commitment (as digest)":T_UInt256:70)("commitment (as digest)":T_UInt256:72).pay(DLC_Int 0).timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again("B":T_Address:14))()().pay(DLC_Int 0){
          const "prim":T_UInt256:80 = TXN_VALUE();
          const "prim":T_Bool:81 = PEQ(DLC_Int 0,"prim":T_UInt256:80);
          claim(CT_Require)("prim":T_Bool:81);
          const "prim":T_UInt256:82 = BALANCE();
          transfer.("prim":T_UInt256:82).to("B":T_Address:14);
          commit();
          exit(DLC_Bytes "Alice quits"); } })){
        const "prim":T_UInt256:73 = TXN_VALUE();
        const "prim":T_Bool:74 = PEQ(DLC_Int 0,"prim":T_UInt256:73);
        claim(CT_Require)("prim":T_Bool:74);
        commit();
        only("B") {
          let "handB (as clo app)":T_UInt256:85;
          const "s (as interact)":T_Bytes:86 = interact("B")."getHand"();
          const "rockP (as prim)":T_Bool:87 = BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "ROCK");
          const "paperP (as prim)":T_Bool:88 = BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "PAPER");
          const "scissorsP (as prim)":T_Bool:89 = BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "SCISSORS");
          const "handB (as prim)":T_Bool:91 = IF_THEN_ELSE("rockP (as prim)":T_Bool:87,DLC_Bool True,"paperP (as prim)":T_Bool:88);
          const "handB (as prim)":T_Bool:93 = IF_THEN_ELSE("handB (as prim)":T_Bool:91,DLC_Bool True,"scissorsP (as prim)":T_Bool:89);
          claim(CT_Assume)("handB (as prim)":T_Bool:93);
          if "rockP (as prim)":T_Bool:87 then {
            "handB (as clo app)":T_UInt256:85 = DLC_Int 0;
             }
          else {
            if "paperP (as prim)":T_Bool:88 then {
              "handB (as clo app)":T_UInt256:85 = DLC_Int 1;
               }
            else {
              "handB (as clo app)":T_UInt256:85 = DLC_Int 2;
               };
             };
          const "handB (as prim)":T_Bool:96 = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:85);
          const "handB (as prim)":T_Bool:97 = PLT("handB (as clo app)":T_UInt256:85,DLC_Int 3);
          const "handB (as prim)":T_Bool:99 = IF_THEN_ELSE("handB (as prim)":T_Bool:96,"handB (as prim)":T_Bool:97,DLC_Bool False);
          claim(CT_Assert)("handB (as prim)":T_Bool:99);
          const "interact":T_Null:100 = interact("B")."shows"();
           };
        claim(CT_Require)(DLC_Bool True);
        only("B") {
           };
        publish("B", again("B":T_Address:14))("handB (as clo app)":T_UInt256:85)("handB (as clo app)":T_UInt256:101).pay(DLC_Int 0).timeout((DLC_Int 10, {
          only("A") {
             };
          publish("A", again("A":T_Address:7))()().pay(DLC_Int 0){
            const "prim":T_UInt256:109 = TXN_VALUE();
            const "prim":T_Bool:110 = PEQ(DLC_Int 0,"prim":T_UInt256:109);
            claim(CT_Require)("prim":T_Bool:110);
            const "prim":T_UInt256:111 = BALANCE();
            transfer.("prim":T_UInt256:111).to("A":T_Address:7);
            commit();
            exit(DLC_Bytes "Bob quits"); } })){
          const "prim":T_UInt256:102 = TXN_VALUE();
          const "prim":T_Bool:103 = PEQ(DLC_Int 0,"prim":T_UInt256:102);
          claim(CT_Require)("prim":T_Bool:103);
          const "prim":T_Bool:113 = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
          const "prim":T_Bool:114 = PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
          const "prim":T_Bool:116 = IF_THEN_ELSE("prim":T_Bool:113,"prim":T_Bool:114,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:116);
          commit();
          only("A") {
            let "clo app":T_Bytes:118;
            const "prim":T_Bool:120 = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:121 = PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:123 = IF_THEN_ELSE("prim":T_Bool:120,"prim":T_Bool:121,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:123);
            const "prim":T_Bool:124 = PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 0);
            if "prim":T_Bool:124 then {
              "clo app":T_Bytes:118 = DLC_Bytes "ROCK";
               }
            else {
              const "prim":T_Bool:125 = PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 1);
              if "prim":T_Bool:125 then {
                "clo app":T_Bytes:118 = DLC_Bytes "PAPER";
                 }
              else {
                "clo app":T_Bytes:118 = DLC_Bytes "SCISSORS";
                 };
               };
            const "interact":T_Null:126 = interact("A")."reveals"("clo app":T_Bytes:118);
             };
          claim(CT_Require)(DLC_Bool True);
          only("A") {
             };
          publish("A", again("A":T_Address:7))("salt (as interact)":T_UInt256:69,"_handA (as clo app)":T_UInt256:53)("salt (as interact)":T_UInt256:127, "_handA (as clo app)":T_UInt256:128).pay(DLC_Int 0).timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again("B":T_Address:14))()().pay(DLC_Int 0){
              const "prim":T_UInt256:136 = TXN_VALUE();
              const "prim":T_Bool:137 = PEQ(DLC_Int 0,"prim":T_UInt256:136);
              claim(CT_Require)("prim":T_Bool:137);
              const "prim":T_UInt256:138 = BALANCE();
              transfer.("prim":T_UInt256:138).to("B":T_Address:14);
              commit();
              exit(DLC_Bytes "Alice quits"); } })){
            const "prim":T_UInt256:129 = TXN_VALUE();
            const "prim":T_Bool:130 = PEQ(DLC_Int 0,"prim":T_UInt256:129);
            claim(CT_Require)("prim":T_Bool:130);
            const "digest":T_UInt256:140 = digest("salt (as interact)":T_UInt256:127,"_handA (as clo app)":T_UInt256:128);
            const "prim":T_Bool:141 = PEQ("commitment (as digest)":T_UInt256:72,"digest":T_UInt256:140);
            claim(CT_Require)("prim":T_Bool:141);
            const "prim":T_Bool:143 = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
            const "prim":T_Bool:144 = PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
            const "prim":T_Bool:146 = IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:146);
            let "roundOutcome (as clo app)":T_UInt256:148;
            const "validA (as prim)":T_Bool:150 = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
            const "validA (as prim)":T_Bool:151 = PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
            const "validA (as prim)":T_Bool:153 = IF_THEN_ELSE("validA (as prim)":T_Bool:150,"validA (as prim)":T_Bool:151,DLC_Bool False);
            const "validB (as prim)":T_Bool:155 = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "validB (as prim)":T_Bool:156 = PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "validB (as prim)":T_Bool:158 = IF_THEN_ELSE("validB (as prim)":T_Bool:155,"validB (as prim)":T_Bool:156,DLC_Bool False);
            const "roundOutcome (as prim)":T_Bool:160 = IF_THEN_ELSE("validA (as prim)":T_Bool:153,"validB (as prim)":T_Bool:158,DLC_Bool False);
            if "roundOutcome (as prim)":T_Bool:160 then {
              const "roundOutcome (as prim)":T_UInt256:161 = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:101);
              const "roundOutcome (as prim)":T_UInt256:162 = ADD("_handA (as clo app)":T_UInt256:128,"roundOutcome (as prim)":T_UInt256:161);
              const "roundOutcome (as prim)":T_UInt256:163 = MOD("roundOutcome (as prim)":T_UInt256:162,DLC_Int 3);
              "roundOutcome (as clo app)":T_UInt256:148 = "roundOutcome (as prim)":T_UInt256:163;
               }
            else {
              if "validA (as prim)":T_Bool:153 then {
                "roundOutcome (as clo app)":T_UInt256:148 = DLC_Int 2;
                 }
              else {
                if "validB (as prim)":T_Bool:158 then {
                  "roundOutcome (as clo app)":T_UInt256:148 = DLC_Int 0;
                   }
                else {
                  "roundOutcome (as clo app)":T_UInt256:148 = DLC_Int 1;
                   };
                 };
               };
            const "roundOutcome (as prim)":T_Bool:166 = PLE(DLC_Int 0,"roundOutcome (as clo app)":T_UInt256:148);
            const "roundOutcome (as prim)":T_Bool:167 = PLT("roundOutcome (as clo app)":T_UInt256:148,DLC_Int 5);
            const "roundOutcome (as prim)":T_Bool:169 = IF_THEN_ELSE("roundOutcome (as prim)":T_Bool:166,"roundOutcome (as prim)":T_Bool:167,DLC_Bool False);
            claim(CT_Assert)("roundOutcome (as prim)":T_Bool:169);
            const "prim":T_Bool:170 = PEQ("roundOutcome (as clo app)":T_UInt256:148,DLC_Int 2);
            const "prim":T_Bool:172 = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
            const "prim":T_Bool:173 = PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
            const "prim":T_Bool:175 = IF_THEN_ELSE("prim":T_Bool:172,"prim":T_Bool:173,DLC_Bool False);
            const "prim":T_Bool:178 = IF_THEN_ELSE("prim":T_Bool:170,DLC_Bool False,DLC_Bool True);
            const "prim":T_Bool:180 = IF_THEN_ELSE("prim":T_Bool:178,DLC_Bool True,"prim":T_Bool:175);
            claim(CT_Assert)("prim":T_Bool:180);
            const "prim":T_Bool:181 = PEQ("roundOutcome (as clo app)":T_UInt256:148,DLC_Int 0);
            const "prim":T_Bool:183 = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:184 = PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:186 = IF_THEN_ELSE("prim":T_Bool:183,"prim":T_Bool:184,DLC_Bool False);
            const "prim":T_Bool:189 = IF_THEN_ELSE("prim":T_Bool:181,DLC_Bool False,DLC_Bool True);
            const "prim":T_Bool:191 = IF_THEN_ELSE("prim":T_Bool:189,DLC_Bool True,"prim":T_Bool:186);
            claim(CT_Assert)("prim":T_Bool:191);
            const "prim":T_Bool:193 = PEQ("roundOutcome (as clo app)":T_UInt256:148,DLC_Int 2);
            const "prim":T_Bool:196 = PEQ("_handA (as clo app)":T_UInt256:128,DLC_Int 0);
            const "prim":T_Bool:198 = IF_THEN_ELSE("prim":T_Bool:196,"prim":T_Bool:193,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:198);
            const "prim":T_Bool:200 = PEQ("_handA (as clo app)":T_UInt256:128,DLC_Int 1);
            const "prim":T_Bool:202 = IF_THEN_ELSE("prim":T_Bool:200,"prim":T_Bool:193,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:202);
            const "prim":T_Bool:204 = PEQ("_handA (as clo app)":T_UInt256:128,DLC_Int 2);
            const "prim":T_Bool:206 = IF_THEN_ELSE("prim":T_Bool:204,"prim":T_Bool:193,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:206);
            const "prim":T_Bool:207 = PEQ("roundOutcome (as clo app)":T_UInt256:148,DLC_Int 0);
            const "prim":T_Bool:210 = PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 0);
            const "prim":T_Bool:212 = IF_THEN_ELSE("prim":T_Bool:210,"prim":T_Bool:207,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:212);
            const "prim":T_Bool:214 = PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 1);
            const "prim":T_Bool:216 = IF_THEN_ELSE("prim":T_Bool:214,"prim":T_Bool:207,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:216);
            const "prim":T_Bool:218 = PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 2);
            const "prim":T_Bool:220 = IF_THEN_ELSE("prim":T_Bool:218,"prim":T_Bool:207,DLC_Bool False);
            claim(CT_Possible)("prim":T_Bool:220);
            const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221 = ADD(DLC_Int 1,"count":T_UInt256:25);
            {
              "count":T_UInt256:25 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221,
              "outcome":T_UInt256:26 = "roundOutcome (as clo app)":T_UInt256:148}
            continue; } } } }
    const "prim":T_Bool:223 = PEQ("outcome":T_UInt256:26,DLC_Int 1);
    const "prim":T_Bool:225 = IF_THEN_ELSE("prim":T_Bool:223,DLC_Bool False,DLC_Bool True);
    claim(CT_Assert)("prim":T_Bool:225);
    let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226;
    const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 = PEQ("outcome":T_UInt256:26,DLC_Int 2);
    if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 then {
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228 = MUL(DLC_Int 2,"array idx":T_UInt256:5);
      "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228,DLC_Int 0];
       }
    else {
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 = PEQ("outcome":T_UInt256:26,DLC_Int 0);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 = MUL(DLC_Int 2,"array idx":T_UInt256:5);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230];
         }
      else {
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
         };
       };
    const "array idx":T_UInt256:231 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226[DLC_Int 0];
    const "array idx":T_UInt256:232 = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226[DLC_Int 1];
    const "prim":T_UInt256:233 = ADD("array idx":T_UInt256:6,"array idx":T_UInt256:231);
    transfer.("prim":T_UInt256:233).to("A":T_Address:7);
    transfer.("array idx":T_UInt256:232).to("B":T_Address:14);
    commit();
    let "clo app":T_Bytes:234;
    const "prim":T_Bool:236 = PLE(DLC_Int 0,"outcome":T_UInt256:26);
    const "prim":T_Bool:237 = PLT("outcome":T_UInt256:26,DLC_Int 5);
    const "prim":T_Bool:239 = IF_THEN_ELSE("prim":T_Bool:236,"prim":T_Bool:237,DLC_Bool False);
    claim(CT_Require)("prim":T_Bool:239);
    const "prim":T_Bool:240 = PEQ("outcome":T_UInt256:26,DLC_Int 0);
    if "prim":T_Bool:240 then {
      "clo app":T_Bytes:234 = DLC_Bytes "Bob wins";
       }
    else {
      const "prim":T_Bool:241 = PEQ("outcome":T_UInt256:26,DLC_Int 1);
      if "prim":T_Bool:241 then {
        "clo app":T_Bytes:234 = DLC_Bytes "Draw";
         }
      else {
        const "prim":T_Bool:242 = PEQ("outcome":T_UInt256:26,DLC_Int 2);
        if "prim":T_Bool:242 then {
          "clo app":T_Bytes:234 = DLC_Bytes "Alice wins";
           }
        else {
          const "prim":T_Bool:243 = PEQ("outcome":T_UInt256:26,DLC_Int 3);
          if "prim":T_Bool:243 then {
            "clo app":T_Bytes:234 = DLC_Bytes "Alice quits";
             }
          else {
            "clo app":T_Bytes:234 = DLC_Bytes "Bob quits";
             };
           };
         };
       };
    exit("clo app":T_Bytes:234); } }