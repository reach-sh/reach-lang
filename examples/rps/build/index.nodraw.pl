#lang pl
{
  "A" = interact {
    commits = T_Fun [] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Array [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    reveals = T_Fun [T_Bytes] T_Null};
  const "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2 PL_Many = interact("A")."getParams"();
  const "array idx":T_UInt256:3 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 0];
  const "array idx":T_UInt256:4 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 1];
  
  const "prim":T_UInt256:8 PL_Once = CP ADD("array idx":T_UInt256:3,"array idx":T_UInt256:4);
  
  sendrecv join("A":T_Address:7) 1 (.publish(("array idx":T_UInt256:3,"array idx":T_UInt256:4), "prim":T_UInt256:8, ()))("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = CP ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = CP TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = CP PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    claim(CT_Require)(DLC_Bool True);
    sendrecv join("B":T_Address:14) 2 ()().timeout(DLC_Int 10, {
      
      sendrecv again("A":T_Address:7) 10 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6)))(){
        const "prim":T_UInt256:22 PL_Once = CP TXN_VALUE();
        const "prim":T_Bool:23 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:22);
        claim(CT_Require)("prim":T_Bool:23);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:15 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = CP PEQ("array idx":T_UInt256:5,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      loopvar {
        "count":T_UInt256:25 = DLC_Int 0,
        "outcome":T_UInt256:26 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:50 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
         "prim":T_Bool:50) }
      {
        let "_handA (as clo app)":T_UInt256:53;
        const "s (as interact)":T_Bytes:54 PL_Many = interact("A")."getHand"();
        const "rockP (as prim)":T_Bool:55 PL_Many = CP BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:56 PL_Many = CP BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:57 PL_Once = CP BYTES_EQ("s (as interact)":T_Bytes:54,DLC_Bytes "SCISSORS");
        const "_handA (as prim)":T_Bool:59 PL_Once = CP IF_THEN_ELSE("rockP (as prim)":T_Bool:55,DLC_Bool True,"paperP (as prim)":T_Bool:56);
        const "_handA (as prim)":T_Bool:61 PL_Once = CP IF_THEN_ELSE("_handA (as prim)":T_Bool:59,DLC_Bool True,"scissorsP (as prim)":T_Bool:57);
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
        const "salt (as prim)":T_UInt256:69 PL_Many = RANDOM();
        const "commitment (as digest)":T_UInt256:70 PL_Once = digest("salt (as prim)":T_UInt256:69,"_handA (as clo app)":T_UInt256:53);
        eff interact("A")."commits"();
        
        claim(CT_Require)(DLC_Bool True);
        
        sendrecv again("A":T_Address:7) 4 (.publish(("commitment (as digest)":T_UInt256:70), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "count":T_UInt256:25)))("commitment (as digest)":T_UInt256:72).timeout(DLC_Int 10, {
          sendrecv again("B":T_Address:14) 9 ()(){
            const "prim":T_UInt256:80 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:81 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:80);
            claim(CT_Require)("prim":T_Bool:81);
            exit(DLC_Bytes "Alice quits"); } }){
          const "prim":T_UInt256:73 PL_Once = CP TXN_VALUE();
          const "prim":T_Bool:74 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:73);
          claim(CT_Require)("prim":T_Bool:74);
          claim(CT_Require)(DLC_Bool True);
          sendrecv again("B":T_Address:14) 5 ()("handB (as clo app)":T_UInt256:101).timeout(DLC_Int 10, {
            
            sendrecv again("A":T_Address:7) 8 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "commitment (as digest)":T_UInt256:72, "count":T_UInt256:25)))(){
              const "prim":T_UInt256:109 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:110 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:109);
              claim(CT_Require)("prim":T_Bool:110);
              exit(DLC_Bytes "Bob quits"); } }){
            const "prim":T_UInt256:102 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:103 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:102);
            claim(CT_Require)("prim":T_Bool:103);
            const "prim":T_Bool:113 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:114 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:116 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:113,"prim":T_Bool:114,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:116);
            let "clo app":T_Bytes:118;
            const "prim":T_Bool:120 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:121 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:123 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:120,"prim":T_Bool:121,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:123);
            const "prim":T_Bool:124 PL_Once = CP PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 0);
            if "prim":T_Bool:124 then {
              "clo app":T_Bytes:118 = DLC_Bytes "ROCK";
               }
            else {
              const "prim":T_Bool:125 PL_Once = CP PEQ("handB (as clo app)":T_UInt256:101,DLC_Int 1);
              if "prim":T_Bool:125 then {
                "clo app":T_Bytes:118 = DLC_Bytes "PAPER";
                 }
              else {
                "clo app":T_Bytes:118 = DLC_Bytes "SCISSORS";
                 };
               };
            eff interact("A")."reveals"("clo app":T_Bytes:118);
            
            claim(CT_Require)(DLC_Bool True);
            
            sendrecv again("A":T_Address:7) 6 (.publish(("salt (as prim)":T_UInt256:69,"_handA (as clo app)":T_UInt256:53), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "commitment (as digest)":T_UInt256:72, "handB (as clo app)":T_UInt256:101, "count":T_UInt256:25)))("salt (as prim)":T_UInt256:127, "_handA (as clo app)":T_UInt256:128).timeout(DLC_Int 10, {
              sendrecv again("B":T_Address:14) 7 ()(){
                const "prim":T_UInt256:136 PL_Once = CP TXN_VALUE();
                const "prim":T_Bool:137 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:136);
                claim(CT_Require)("prim":T_Bool:137);
                exit(DLC_Bytes "Alice quits"); } }){
              const "prim":T_UInt256:129 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:130 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:129);
              claim(CT_Require)("prim":T_Bool:130);
              const "digest":T_UInt256:140 PL_Once = digest("salt (as prim)":T_UInt256:127,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:141 PL_Once = CP PEQ("commitment (as digest)":T_UInt256:72,"digest":T_UInt256:140);
              claim(CT_Require)("prim":T_Bool:141);
              const "prim":T_Bool:143 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:144 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "prim":T_Bool:146 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:146);
              let "roundOutcome (as clo app)":T_UInt256:148;
              const "validA (as prim)":T_Bool:150 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "validA (as prim)":T_Bool:151 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "validA (as prim)":T_Bool:153 PL_Many = CP IF_THEN_ELSE("validA (as prim)":T_Bool:150,"validA (as prim)":T_Bool:151,DLC_Bool False);
              const "validB (as prim)":T_Bool:155 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
              const "validB (as prim)":T_Bool:156 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
              const "validB (as prim)":T_Bool:158 PL_Many = CP IF_THEN_ELSE("validB (as prim)":T_Bool:155,"validB (as prim)":T_Bool:156,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:160 PL_Once = CP IF_THEN_ELSE("validA (as prim)":T_Bool:153,"validB (as prim)":T_Bool:158,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:160 then {
                const "roundOutcome (as prim)":T_UInt256:161 PL_Once = CP SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:101);
                const "roundOutcome (as prim)":T_UInt256:162 PL_Once = CP ADD("_handA (as clo app)":T_UInt256:128,"roundOutcome (as prim)":T_UInt256:161);
                const "roundOutcome (as prim)":T_UInt256:163 PL_Once = CP MOD("roundOutcome (as prim)":T_UInt256:162,DLC_Int 3);
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
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221 PL_Once = CP ADD(DLC_Int 1,"count":T_UInt256:25);
              {
                "count":T_UInt256:25 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221,
                "outcome":T_UInt256:26 = "roundOutcome (as clo app)":T_UInt256:148}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
           };
         };
      let "clo app":T_Bytes:234;
      const "prim":T_Bool:236 PL_Once = CP PLE(DLC_Int 0,"outcome":T_UInt256:26);
      const "prim":T_Bool:237 PL_Once = CP PLT("outcome":T_UInt256:26,DLC_Int 5);
      const "prim":T_Bool:239 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:236,"prim":T_Bool:237,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:239);
      const "prim":T_Bool:240 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
      if "prim":T_Bool:240 then {
        "clo app":T_Bytes:234 = DLC_Bytes "Bob wins";
         }
      else {
        const "prim":T_Bool:241 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
        if "prim":T_Bool:241 then {
          "clo app":T_Bytes:234 = DLC_Bytes "Draw";
           }
        else {
          const "prim":T_Bool:242 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
          if "prim":T_Bool:242 then {
            "clo app":T_Bytes:234 = DLC_Bytes "Alice wins";
             }
          else {
            const "prim":T_Bool:243 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 3);
            if "prim":T_Bool:243 then {
              "clo app":T_Bytes:234 = DLC_Bytes "Alice quits";
               }
            else {
              "clo app":T_Bytes:234 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      exit("clo app":T_Bytes:234); } },
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    shows = T_Fun [] T_Null};
  sendrecv join("A":T_Address:7) 1 ()("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = CP ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = CP TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = CP PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    eff interact("B")."acceptParams"("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    
    claim(CT_Require)(DLC_Bool True);
    
    sendrecv join("B":T_Address:14) 2 (.publish((), "array idx":T_UInt256:5, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6)))().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:7) 10 ()(){
        const "prim":T_UInt256:22 PL_Once = CP TXN_VALUE();
        const "prim":T_Bool:23 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:22);
        claim(CT_Require)("prim":T_Bool:23);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:15 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = CP PEQ("array idx":T_UInt256:5,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      loopvar {
        "count":T_UInt256:25 = DLC_Int 0,
        "outcome":T_UInt256:26 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:50 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
         "prim":T_Bool:50) }
      {
        claim(CT_Require)(DLC_Bool True);
        sendrecv again("A":T_Address:7) 4 ()("commitment (as digest)":T_UInt256:72).timeout(DLC_Int 10, {
          
          sendrecv again("B":T_Address:14) 9 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "count":T_UInt256:25)))(){
            const "prim":T_UInt256:80 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:81 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:80);
            claim(CT_Require)("prim":T_Bool:81);
            exit(DLC_Bytes "Alice quits"); } }){
          const "prim":T_UInt256:73 PL_Once = CP TXN_VALUE();
          const "prim":T_Bool:74 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:73);
          claim(CT_Require)("prim":T_Bool:74);
          let "handB (as clo app)":T_UInt256:85;
          const "s (as interact)":T_Bytes:86 PL_Many = interact("B")."getHand"();
          const "rockP (as prim)":T_Bool:87 PL_Many = CP BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "ROCK");
          const "paperP (as prim)":T_Bool:88 PL_Many = CP BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "PAPER");
          const "scissorsP (as prim)":T_Bool:89 PL_Once = CP BYTES_EQ("s (as interact)":T_Bytes:86,DLC_Bytes "SCISSORS");
          const "handB (as prim)":T_Bool:91 PL_Once = CP IF_THEN_ELSE("rockP (as prim)":T_Bool:87,DLC_Bool True,"paperP (as prim)":T_Bool:88);
          const "handB (as prim)":T_Bool:93 PL_Once = CP IF_THEN_ELSE("handB (as prim)":T_Bool:91,DLC_Bool True,"scissorsP (as prim)":T_Bool:89);
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
          eff interact("B")."shows"();
          
          claim(CT_Require)(DLC_Bool True);
          
          sendrecv again("B":T_Address:14) 5 (.publish(("handB (as clo app)":T_UInt256:85), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "commitment (as digest)":T_UInt256:72, "count":T_UInt256:25)))("handB (as clo app)":T_UInt256:101).timeout(DLC_Int 10, {
            sendrecv again("A":T_Address:7) 8 ()(){
              const "prim":T_UInt256:109 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:110 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:109);
              claim(CT_Require)("prim":T_Bool:110);
              exit(DLC_Bytes "Bob quits"); } }){
            const "prim":T_UInt256:102 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:103 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:102);
            claim(CT_Require)("prim":T_Bool:103);
            const "prim":T_Bool:113 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:114 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:116 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:113,"prim":T_Bool:114,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:116);
            claim(CT_Require)(DLC_Bool True);
            sendrecv again("A":T_Address:7) 6 ()("salt (as prim)":T_UInt256:127, "_handA (as clo app)":T_UInt256:128).timeout(DLC_Int 10, {
              
              sendrecv again("B":T_Address:14) 7 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:14, "commitment (as digest)":T_UInt256:72, "handB (as clo app)":T_UInt256:101, "count":T_UInt256:25)))(){
                const "prim":T_UInt256:136 PL_Once = CP TXN_VALUE();
                const "prim":T_Bool:137 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:136);
                claim(CT_Require)("prim":T_Bool:137);
                exit(DLC_Bytes "Alice quits"); } }){
              const "prim":T_UInt256:129 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:130 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:129);
              claim(CT_Require)("prim":T_Bool:130);
              const "digest":T_UInt256:140 PL_Once = digest("salt (as prim)":T_UInt256:127,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:141 PL_Once = CP PEQ("commitment (as digest)":T_UInt256:72,"digest":T_UInt256:140);
              claim(CT_Require)("prim":T_Bool:141);
              const "prim":T_Bool:143 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:144 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "prim":T_Bool:146 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:146);
              let "roundOutcome (as clo app)":T_UInt256:148;
              const "validA (as prim)":T_Bool:150 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "validA (as prim)":T_Bool:151 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "validA (as prim)":T_Bool:153 PL_Many = CP IF_THEN_ELSE("validA (as prim)":T_Bool:150,"validA (as prim)":T_Bool:151,DLC_Bool False);
              const "validB (as prim)":T_Bool:155 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
              const "validB (as prim)":T_Bool:156 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
              const "validB (as prim)":T_Bool:158 PL_Many = CP IF_THEN_ELSE("validB (as prim)":T_Bool:155,"validB (as prim)":T_Bool:156,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:160 PL_Once = CP IF_THEN_ELSE("validA (as prim)":T_Bool:153,"validB (as prim)":T_Bool:158,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:160 then {
                const "roundOutcome (as prim)":T_UInt256:161 PL_Once = CP SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:101);
                const "roundOutcome (as prim)":T_UInt256:162 PL_Once = CP ADD("_handA (as clo app)":T_UInt256:128,"roundOutcome (as prim)":T_UInt256:161);
                const "roundOutcome (as prim)":T_UInt256:163 PL_Once = CP MOD("roundOutcome (as prim)":T_UInt256:162,DLC_Int 3);
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
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221 PL_Once = CP ADD(DLC_Int 1,"count":T_UInt256:25);
              {
                "count":T_UInt256:25 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221,
                "outcome":T_UInt256:26 = "roundOutcome (as clo app)":T_UInt256:148}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
           };
         };
      let "clo app":T_Bytes:234;
      const "prim":T_Bool:236 PL_Once = CP PLE(DLC_Int 0,"outcome":T_UInt256:26);
      const "prim":T_Bool:237 PL_Once = CP PLT("outcome":T_UInt256:26,DLC_Int 5);
      const "prim":T_Bool:239 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:236,"prim":T_Bool:237,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:239);
      const "prim":T_Bool:240 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
      if "prim":T_Bool:240 then {
        "clo app":T_Bytes:234 = DLC_Bytes "Bob wins";
         }
      else {
        const "prim":T_Bool:241 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
        if "prim":T_Bool:241 then {
          "clo app":T_Bytes:234 = DLC_Bytes "Draw";
           }
        else {
          const "prim":T_Bool:242 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
          if "prim":T_Bool:242 then {
            "clo app":T_Bytes:234 = DLC_Bytes "Alice wins";
             }
          else {
            const "prim":T_Bool:243 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 3);
            if "prim":T_Bool:243 then {
              "clo app":T_Bytes:234 = DLC_Bytes "Alice quits";
               }
            else {
              "clo app":T_Bytes:234 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      exit("clo app":T_Bytes:234); } },
  "O" = interact {
    };
  sendrecv join("A":T_Address:7) 1 ()("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = CP ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = CP TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = CP PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    claim(CT_Require)(DLC_Bool True);
    sendrecv join("B":T_Address:14) 2 ()().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:7) 10 ()(){
        const "prim":T_UInt256:22 PL_Once = CP TXN_VALUE();
        const "prim":T_Bool:23 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:22);
        claim(CT_Require)("prim":T_Bool:23);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:15 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = CP PEQ("array idx":T_UInt256:5,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      loopvar {
        "count":T_UInt256:25 = DLC_Int 0,
        "outcome":T_UInt256:26 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:50 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
         "prim":T_Bool:50) }
      {
        claim(CT_Require)(DLC_Bool True);
        sendrecv again("A":T_Address:7) 4 ()("commitment (as digest)":T_UInt256:72).timeout(DLC_Int 10, {
          sendrecv again("B":T_Address:14) 9 ()(){
            const "prim":T_UInt256:80 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:81 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:80);
            claim(CT_Require)("prim":T_Bool:81);
            exit(DLC_Bytes "Alice quits"); } }){
          const "prim":T_UInt256:73 PL_Once = CP TXN_VALUE();
          const "prim":T_Bool:74 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:73);
          claim(CT_Require)("prim":T_Bool:74);
          claim(CT_Require)(DLC_Bool True);
          sendrecv again("B":T_Address:14) 5 ()("handB (as clo app)":T_UInt256:101).timeout(DLC_Int 10, {
            sendrecv again("A":T_Address:7) 8 ()(){
              const "prim":T_UInt256:109 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:110 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:109);
              claim(CT_Require)("prim":T_Bool:110);
              exit(DLC_Bytes "Bob quits"); } }){
            const "prim":T_UInt256:102 PL_Once = CP TXN_VALUE();
            const "prim":T_Bool:103 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:102);
            claim(CT_Require)("prim":T_Bool:103);
            const "prim":T_Bool:113 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
            const "prim":T_Bool:114 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
            const "prim":T_Bool:116 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:113,"prim":T_Bool:114,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:116);
            claim(CT_Require)(DLC_Bool True);
            sendrecv again("A":T_Address:7) 6 ()("salt (as prim)":T_UInt256:127, "_handA (as clo app)":T_UInt256:128).timeout(DLC_Int 10, {
              sendrecv again("B":T_Address:14) 7 ()(){
                const "prim":T_UInt256:136 PL_Once = CP TXN_VALUE();
                const "prim":T_Bool:137 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:136);
                claim(CT_Require)("prim":T_Bool:137);
                exit(DLC_Bytes "Alice quits"); } }){
              const "prim":T_UInt256:129 PL_Once = CP TXN_VALUE();
              const "prim":T_Bool:130 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:129);
              claim(CT_Require)("prim":T_Bool:130);
              const "digest":T_UInt256:140 PL_Once = digest("salt (as prim)":T_UInt256:127,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:141 PL_Once = CP PEQ("commitment (as digest)":T_UInt256:72,"digest":T_UInt256:140);
              claim(CT_Require)("prim":T_Bool:141);
              const "prim":T_Bool:143 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "prim":T_Bool:144 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "prim":T_Bool:146 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:146);
              let "roundOutcome (as clo app)":T_UInt256:148;
              const "validA (as prim)":T_Bool:150 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
              const "validA (as prim)":T_Bool:151 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
              const "validA (as prim)":T_Bool:153 PL_Many = CP IF_THEN_ELSE("validA (as prim)":T_Bool:150,"validA (as prim)":T_Bool:151,DLC_Bool False);
              const "validB (as prim)":T_Bool:155 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
              const "validB (as prim)":T_Bool:156 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
              const "validB (as prim)":T_Bool:158 PL_Many = CP IF_THEN_ELSE("validB (as prim)":T_Bool:155,"validB (as prim)":T_Bool:156,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:160 PL_Once = CP IF_THEN_ELSE("validA (as prim)":T_Bool:153,"validB (as prim)":T_Bool:158,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:160 then {
                const "roundOutcome (as prim)":T_UInt256:161 PL_Once = CP SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:101);
                const "roundOutcome (as prim)":T_UInt256:162 PL_Once = CP ADD("_handA (as clo app)":T_UInt256:128,"roundOutcome (as prim)":T_UInt256:161);
                const "roundOutcome (as prim)":T_UInt256:163 PL_Once = CP MOD("roundOutcome (as prim)":T_UInt256:162,DLC_Int 3);
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
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221 PL_Once = CP ADD(DLC_Int 1,"count":T_UInt256:25);
              {
                "count":T_UInt256:25 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221,
                "outcome":T_UInt256:26 = "roundOutcome (as clo app)":T_UInt256:148}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
           };
         };
      let "clo app":T_Bytes:234;
      const "prim":T_Bool:236 PL_Once = CP PLE(DLC_Int 0,"outcome":T_UInt256:26);
      const "prim":T_Bool:237 PL_Once = CP PLT("outcome":T_UInt256:26,DLC_Int 5);
      const "prim":T_Bool:239 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:236,"prim":T_Bool:237,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:239);
      const "prim":T_Bool:240 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
      if "prim":T_Bool:240 then {
        "clo app":T_Bytes:234 = DLC_Bytes "Bob wins";
         }
      else {
        const "prim":T_Bool:241 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
        if "prim":T_Bool:241 then {
          "clo app":T_Bytes:234 = DLC_Bytes "Draw";
           }
        else {
          const "prim":T_Bool:242 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
          if "prim":T_Bool:242 then {
            "clo app":T_Bytes:234 = DLC_Bytes "Alice wins";
             }
          else {
            const "prim":T_Bool:243 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 3);
            if "prim":T_Bool:243 then {
              "clo app":T_Bytes:234 = DLC_Bytes "Alice quits";
               }
            else {
              "clo app":T_Bytes:234 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      exit("clo app":T_Bytes:234); } }}

{
  1 = {
    join("A":T_Address:7),
    (between [] []),
    last = 0,
    [],
    ["array idx":T_UInt256:5, "array idx":T_UInt256:6],
    {
      const "prim":T_UInt256:9 PL_Once = CP ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
      const "prim":T_UInt256:10 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:11 PL_Once = CP PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
      claim(CT_Require)("prim":T_Bool:11);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6 ]) } },
  2 = {
    join("B":T_Address:14),
    (between [] [DLC_Int 10]),
    last = 1,
    ["A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6],
    [],
    {
      const "prim":T_UInt256:15 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = CP PEQ("array idx":T_UInt256:5,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      (jump! 3 [ "A":T_Address:7
               , "array idx":T_UInt256:5
               , "array idx":T_UInt256:6
               , "B":T_Address:14 ] {
        "count":T_UInt256:25 = DLC_Int 0,
        "outcome":T_UInt256:26 = DLC_Int 1}) } },
  3 = {
    loop!,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14 ],
    ["count":T_UInt256:25, "outcome":T_UInt256:26],
    {
      const "prim":T_Bool:50 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 1);
      
      if "prim":T_Bool:50 then {
        (wait! [ "A":T_Address:7
               , "array idx":T_UInt256:5
               , "array idx":T_UInt256:6
               , "B":T_Address:14
               , "count":T_UInt256:25 ]) }
      else {
        let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226;
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 2);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:227 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:228,DLC_Int 0];
           }
        else {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = CP PEQ("outcome":T_UInt256:26,DLC_Int 0);
          if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = CP MUL(DLC_Int 2,"array idx":T_UInt256:5);
            "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230];
             }
          else {
            "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
             };
           };
        const "array idx":T_UInt256:231 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226[DLC_Int 0];
        const "array idx":T_UInt256:232 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:226[DLC_Int 1];
        const "prim":T_UInt256:233 PL_Once = CP ADD("array idx":T_UInt256:6,"array idx":T_UInt256:231);
        transfer.("prim":T_UInt256:233).to("A":T_Address:7);
        transfer.("array idx":T_UInt256:232).to("B":T_Address:14);
        (halt! ) }; } },
  4 = {
    again("A":T_Address:7),
    (between [] [DLC_Int 10]),
    last = 3,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "count":T_UInt256:25 ],
    ["commitment (as digest)":T_UInt256:72],
    {
      const "prim":T_UInt256:73 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:74 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:73);
      claim(CT_Require)("prim":T_Bool:74);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6
             , "B":T_Address:14
             , "commitment (as digest)":T_UInt256:72
             , "count":T_UInt256:25 ]) } },
  5 = {
    again("B":T_Address:14),
    (between [] [DLC_Int 10]),
    last = 4,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:72
    , "count":T_UInt256:25 ],
    ["handB (as clo app)":T_UInt256:101],
    {
      const "prim":T_UInt256:102 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:103 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:102);
      claim(CT_Require)("prim":T_Bool:103);
      const "prim":T_Bool:113 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
      const "prim":T_Bool:114 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
      const "prim":T_Bool:116 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:113,"prim":T_Bool:114,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:116);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6
             , "B":T_Address:14
             , "commitment (as digest)":T_UInt256:72
             , "handB (as clo app)":T_UInt256:101
             , "count":T_UInt256:25 ]) } },
  6 = {
    again("A":T_Address:7),
    (between [] [DLC_Int 10]),
    last = 5,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:72
    , "handB (as clo app)":T_UInt256:101
    , "count":T_UInt256:25 ],
    ["salt (as prim)":T_UInt256:127, "_handA (as clo app)":T_UInt256:128],
    {
      const "prim":T_UInt256:129 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:130 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:129);
      claim(CT_Require)("prim":T_Bool:130);
      const "digest":T_UInt256:140 PL_Once = digest("salt (as prim)":T_UInt256:127,"_handA (as clo app)":T_UInt256:128);
      const "prim":T_Bool:141 PL_Once = CP PEQ("commitment (as digest)":T_UInt256:72,"digest":T_UInt256:140);
      claim(CT_Require)("prim":T_Bool:141);
      const "prim":T_Bool:143 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
      const "prim":T_Bool:144 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
      const "prim":T_Bool:146 PL_Once = CP IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:146);
      let "roundOutcome (as clo app)":T_UInt256:148;
      const "validA (as prim)":T_Bool:150 PL_Once = CP PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:128);
      const "validA (as prim)":T_Bool:151 PL_Once = CP PLT("_handA (as clo app)":T_UInt256:128,DLC_Int 3);
      const "validA (as prim)":T_Bool:153 PL_Many = CP IF_THEN_ELSE("validA (as prim)":T_Bool:150,"validA (as prim)":T_Bool:151,DLC_Bool False);
      const "validB (as prim)":T_Bool:155 PL_Once = CP PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:101);
      const "validB (as prim)":T_Bool:156 PL_Once = CP PLT("handB (as clo app)":T_UInt256:101,DLC_Int 3);
      const "validB (as prim)":T_Bool:158 PL_Many = CP IF_THEN_ELSE("validB (as prim)":T_Bool:155,"validB (as prim)":T_Bool:156,DLC_Bool False);
      const "roundOutcome (as prim)":T_Bool:160 PL_Once = CP IF_THEN_ELSE("validA (as prim)":T_Bool:153,"validB (as prim)":T_Bool:158,DLC_Bool False);
      if "roundOutcome (as prim)":T_Bool:160 then {
        const "roundOutcome (as prim)":T_UInt256:161 PL_Once = CP SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:101);
        const "roundOutcome (as prim)":T_UInt256:162 PL_Once = CP ADD("_handA (as clo app)":T_UInt256:128,"roundOutcome (as prim)":T_UInt256:161);
        const "roundOutcome (as prim)":T_UInt256:163 PL_Once = CP MOD("roundOutcome (as prim)":T_UInt256:162,DLC_Int 3);
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
      const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221 PL_Once = CP ADD(DLC_Int 1,"count":T_UInt256:25);
      (jump! 3 [ "A":T_Address:7
               , "array idx":T_UInt256:5
               , "array idx":T_UInt256:6
               , "B":T_Address:14 ] {
        "count":T_UInt256:25 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:221,
        "outcome":T_UInt256:26 = "roundOutcome (as clo app)":T_UInt256:148}) } },
  7 = {
    again("B":T_Address:14),
    (between [DLC_Int 10] []),
    last = 5,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:72
    , "handB (as clo app)":T_UInt256:101
    , "count":T_UInt256:25 ],
    [],
    {
      const "prim":T_UInt256:136 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:137 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:136);
      claim(CT_Require)("prim":T_Bool:137);
      const "prim":T_UInt256:138 PL_Once = CP BALANCE();
      transfer.("prim":T_UInt256:138).to("B":T_Address:14);
      (halt! ) } },
  8 = {
    again("A":T_Address:7),
    (between [DLC_Int 10] []),
    last = 4,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:72
    , "count":T_UInt256:25 ],
    [],
    {
      const "prim":T_UInt256:109 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:110 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:109);
      claim(CT_Require)("prim":T_Bool:110);
      const "prim":T_UInt256:111 PL_Once = CP BALANCE();
      transfer.("prim":T_UInt256:111).to("A":T_Address:7);
      (halt! ) } },
  9 = {
    again("B":T_Address:14),
    (between [DLC_Int 10] []),
    last = 3,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:14
    , "count":T_UInt256:25 ],
    [],
    {
      const "prim":T_UInt256:80 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:81 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:80);
      claim(CT_Require)("prim":T_Bool:81);
      const "prim":T_UInt256:82 PL_Once = CP BALANCE();
      transfer.("prim":T_UInt256:82).to("B":T_Address:14);
      (halt! ) } },
  10 = {
    again("A":T_Address:7),
    (between [DLC_Int 10] []),
    last = 1,
    ["A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6],
    [],
    {
      const "prim":T_UInt256:22 PL_Once = CP TXN_VALUE();
      const "prim":T_Bool:23 PL_Once = CP PEQ(DLC_Int 0,"prim":T_UInt256:22);
      claim(CT_Require)("prim":T_Bool:23);
      const "prim":T_UInt256:24 PL_Once = CP BALANCE();
      transfer.("prim":T_UInt256:24).to("A":T_Address:7);
      (halt! ) } }}