#lang pl
{
  "A" = interact {
    commits = T_Fun [] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Array [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    reveals = T_Fun [T_Bytes] T_Null};
  const "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2 PL_Many = interact("A")."getParams"();
  const "array idx":T_UInt256:3 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 0];
  const "array idx":T_UInt256:4 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Array [T_UInt256,T_UInt256]:2[DLC_Int 1];
  
  const "prim":T_UInt256:8 PL_Once = ADD("array idx":T_UInt256:3,"array idx":T_UInt256:4);
  
  sendrecv join("A":T_Address:7) 1 (.publish(("array idx":T_UInt256:3,"array idx":T_UInt256:4), "prim":T_UInt256:8, ()))("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    claim(CT_Require)(DLC_Bool True);
    sendrecv join("B":T_Address:15) 2 ()().timeout(DLC_Int 10, {
      
      sendrecv again("A":T_Address:7) 9 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6)))(){
        const "prim":T_UInt256:23 PL_Once = TXN_VALUE();
        const "prim":T_Bool:24 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:23);
        claim(CT_Require)("prim":T_Bool:24);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:16 PL_Once = TXN_VALUE();
      const "prim":T_Bool:17 PL_Once = PEQ("array idx":T_UInt256:5,"prim":T_UInt256:16);
      claim(CT_Require)("prim":T_Bool:17);
      eff interact("A")."partnerIs"("B":T_Address:15);
      let "_handA (as clo app)":T_UInt256:29;
      const "s (as interact)":T_Bytes:30 PL_Many = interact("A")."getHand"();
      const "rockP (as prim)":T_Bool:31 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "ROCK");
      const "paperP (as prim)":T_Bool:32 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "PAPER");
      const "scissorsP (as prim)":T_Bool:33 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:30,DLC_Bytes "SCISSORS");
      const "_handA (as prim)":T_Bool:35 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:31,DLC_Bool True,"paperP (as prim)":T_Bool:32);
      const "_handA (as prim)":T_Bool:37 PL_Once = IF_THEN_ELSE("_handA (as prim)":T_Bool:35,DLC_Bool True,"scissorsP (as prim)":T_Bool:33);
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
      const "salt (as interact)":T_UInt256:45 PL_Many = interact("A")."random"();
      const "commitment (as digest)":T_UInt256:46 PL_Once = digest("salt (as interact)":T_UInt256:45,"_handA (as clo app)":T_UInt256:29);
      eff interact("A")."commits"();
      
      claim(CT_Require)(DLC_Bool True);
      
      sendrecv again("A":T_Address:7) 3 (.publish(("commitment (as digest)":T_UInt256:46), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15)))("commitment (as digest)":T_UInt256:48).timeout(DLC_Int 10, {
        sendrecv again("B":T_Address:15) 8 ()(){
          const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
          const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
          claim(CT_Require)("prim":T_Bool:57);
          exit(DLC_Bytes "Alice quits"); } }){
        const "prim":T_UInt256:49 PL_Once = TXN_VALUE();
        const "prim":T_Bool:50 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:49);
        claim(CT_Require)("prim":T_Bool:50);
        claim(CT_Require)(DLC_Bool True);
        sendrecv again("B":T_Address:15) 4 ()("handB (as clo app)":T_UInt256:77).timeout(DLC_Int 10, {
          
          sendrecv again("A":T_Address:7) 7 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15, "commitment (as digest)":T_UInt256:48)))(){
            const "prim":T_UInt256:85 PL_Once = TXN_VALUE();
            const "prim":T_Bool:86 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:85);
            claim(CT_Require)("prim":T_Bool:86);
            exit(DLC_Bytes "Bob quits"); } }){
          const "prim":T_UInt256:78 PL_Once = TXN_VALUE();
          const "prim":T_Bool:79 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:78);
          claim(CT_Require)("prim":T_Bool:79);
          const "prim":T_Bool:89 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:90 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:92 PL_Once = IF_THEN_ELSE("prim":T_Bool:89,"prim":T_Bool:90,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:92);
          let "clo app":T_Bytes:94;
          const "prim":T_Bool:96 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:97 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:99 PL_Once = IF_THEN_ELSE("prim":T_Bool:96,"prim":T_Bool:97,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:99);
          const "prim":T_Bool:100 PL_Once = PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 0);
          if "prim":T_Bool:100 then {
            "clo app":T_Bytes:94 = DLC_Bytes "ROCK";
             }
          else {
            const "prim":T_Bool:101 PL_Once = PEQ("handB (as clo app)":T_UInt256:77,DLC_Int 1);
            if "prim":T_Bool:101 then {
              "clo app":T_Bytes:94 = DLC_Bytes "PAPER";
               }
            else {
              "clo app":T_Bytes:94 = DLC_Bytes "SCISSORS";
               };
             };
          eff interact("A")."reveals"("clo app":T_Bytes:94);
          
          claim(CT_Require)(DLC_Bool True);
          
          sendrecv again("A":T_Address:7) 5 (.publish(("salt (as interact)":T_UInt256:45,"_handA (as clo app)":T_UInt256:29), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15, "commitment (as digest)":T_UInt256:48, "handB (as clo app)":T_UInt256:77)))("salt (as interact)":T_UInt256:103, "_handA (as clo app)":T_UInt256:104).timeout(DLC_Int 10, {
            sendrecv again("B":T_Address:15) 6 ()(){
              const "prim":T_UInt256:112 PL_Once = TXN_VALUE();
              const "prim":T_Bool:113 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:112);
              claim(CT_Require)("prim":T_Bool:113);
              exit(DLC_Bytes "Alice quits"); } }){
            const "prim":T_UInt256:105 PL_Once = TXN_VALUE();
            const "prim":T_Bool:106 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:105);
            claim(CT_Require)("prim":T_Bool:106);
            const "digest":T_UInt256:116 PL_Once = digest("salt (as interact)":T_UInt256:103,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:117 PL_Once = PEQ("commitment (as digest)":T_UInt256:48,"digest":T_UInt256:116);
            claim(CT_Require)("prim":T_Bool:117);
            const "prim":T_Bool:119 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:120 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "prim":T_Bool:122 PL_Once = IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:122);
            let "outcome (as clo app)":T_UInt256:124;
            const "validA (as prim)":T_Bool:126 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "validA (as prim)":T_Bool:127 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "validA (as prim)":T_Bool:129 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:126,"validA (as prim)":T_Bool:127,DLC_Bool False);
            const "validB (as prim)":T_Bool:131 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
            const "validB (as prim)":T_Bool:132 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
            const "validB (as prim)":T_Bool:134 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:131,"validB (as prim)":T_Bool:132,DLC_Bool False);
            const "outcome (as prim)":T_Bool:136 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:129,"validB (as prim)":T_Bool:134,DLC_Bool False);
            if "outcome (as prim)":T_Bool:136 then {
              const "outcome (as prim)":T_UInt256:137 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:77);
              const "outcome (as prim)":T_UInt256:138 PL_Once = ADD("_handA (as clo app)":T_UInt256:104,"outcome (as prim)":T_UInt256:137);
              const "outcome (as prim)":T_UInt256:139 PL_Once = MOD("outcome (as prim)":T_UInt256:138,DLC_Int 3);
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
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
                 };
               };
            let "clo app":T_Bytes:205;
            const "prim":T_Bool:207 PL_Once = PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
            const "prim":T_Bool:208 PL_Once = PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
            const "prim":T_Bool:210 PL_Once = IF_THEN_ELSE("prim":T_Bool:207,"prim":T_Bool:208,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:210);
            const "prim":T_Bool:211 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
            if "prim":T_Bool:211 then {
              "clo app":T_Bytes:205 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:212 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 1);
              if "prim":T_Bool:212 then {
                "clo app":T_Bytes:205 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:213 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
                if "prim":T_Bool:213 then {
                  "clo app":T_Bytes:205 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:214 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 3);
                  if "prim":T_Bool:214 then {
                    "clo app":T_Bytes:205 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:205 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            exit("clo app":T_Bytes:205); } } } } },
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null};
  sendrecv join("A":T_Address:7) 1 ()("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    eff interact("B")."partnerIs"("A":T_Address:7);
    eff interact("B")."acceptParams"("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    
    claim(CT_Require)(DLC_Bool True);
    
    sendrecv join("B":T_Address:15) 2 (.publish((), "array idx":T_UInt256:5, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6)))().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:7) 9 ()(){
        const "prim":T_UInt256:23 PL_Once = TXN_VALUE();
        const "prim":T_Bool:24 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:23);
        claim(CT_Require)("prim":T_Bool:24);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:16 PL_Once = TXN_VALUE();
      const "prim":T_Bool:17 PL_Once = PEQ("array idx":T_UInt256:5,"prim":T_UInt256:16);
      claim(CT_Require)("prim":T_Bool:17);
      claim(CT_Require)(DLC_Bool True);
      sendrecv again("A":T_Address:7) 3 ()("commitment (as digest)":T_UInt256:48).timeout(DLC_Int 10, {
        
        sendrecv again("B":T_Address:15) 8 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15)))(){
          const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
          const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
          claim(CT_Require)("prim":T_Bool:57);
          exit(DLC_Bytes "Alice quits"); } }){
        const "prim":T_UInt256:49 PL_Once = TXN_VALUE();
        const "prim":T_Bool:50 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:49);
        claim(CT_Require)("prim":T_Bool:50);
        let "handB (as clo app)":T_UInt256:61;
        const "s (as interact)":T_Bytes:62 PL_Many = interact("B")."getHand"();
        const "rockP (as prim)":T_Bool:63 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:64 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:65 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:62,DLC_Bytes "SCISSORS");
        const "handB (as prim)":T_Bool:67 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:63,DLC_Bool True,"paperP (as prim)":T_Bool:64);
        const "handB (as prim)":T_Bool:69 PL_Once = IF_THEN_ELSE("handB (as prim)":T_Bool:67,DLC_Bool True,"scissorsP (as prim)":T_Bool:65);
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
        eff interact("B")."shows"();
        
        claim(CT_Require)(DLC_Bool True);
        
        sendrecv again("B":T_Address:15) 4 (.publish(("handB (as clo app)":T_UInt256:61), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15, "commitment (as digest)":T_UInt256:48)))("handB (as clo app)":T_UInt256:77).timeout(DLC_Int 10, {
          sendrecv again("A":T_Address:7) 7 ()(){
            const "prim":T_UInt256:85 PL_Once = TXN_VALUE();
            const "prim":T_Bool:86 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:85);
            claim(CT_Require)("prim":T_Bool:86);
            exit(DLC_Bytes "Bob quits"); } }){
          const "prim":T_UInt256:78 PL_Once = TXN_VALUE();
          const "prim":T_Bool:79 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:78);
          claim(CT_Require)("prim":T_Bool:79);
          const "prim":T_Bool:89 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:90 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:92 PL_Once = IF_THEN_ELSE("prim":T_Bool:89,"prim":T_Bool:90,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:92);
          claim(CT_Require)(DLC_Bool True);
          sendrecv again("A":T_Address:7) 5 ()("salt (as interact)":T_UInt256:103, "_handA (as clo app)":T_UInt256:104).timeout(DLC_Int 10, {
            
            sendrecv again("B":T_Address:15) 6 (.publish((), DLC_Int 0, ("A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6, "B":T_Address:15, "commitment (as digest)":T_UInt256:48, "handB (as clo app)":T_UInt256:77)))(){
              const "prim":T_UInt256:112 PL_Once = TXN_VALUE();
              const "prim":T_Bool:113 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:112);
              claim(CT_Require)("prim":T_Bool:113);
              exit(DLC_Bytes "Alice quits"); } }){
            const "prim":T_UInt256:105 PL_Once = TXN_VALUE();
            const "prim":T_Bool:106 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:105);
            claim(CT_Require)("prim":T_Bool:106);
            const "digest":T_UInt256:116 PL_Once = digest("salt (as interact)":T_UInt256:103,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:117 PL_Once = PEQ("commitment (as digest)":T_UInt256:48,"digest":T_UInt256:116);
            claim(CT_Require)("prim":T_Bool:117);
            const "prim":T_Bool:119 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:120 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "prim":T_Bool:122 PL_Once = IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:122);
            let "outcome (as clo app)":T_UInt256:124;
            const "validA (as prim)":T_Bool:126 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "validA (as prim)":T_Bool:127 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "validA (as prim)":T_Bool:129 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:126,"validA (as prim)":T_Bool:127,DLC_Bool False);
            const "validB (as prim)":T_Bool:131 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
            const "validB (as prim)":T_Bool:132 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
            const "validB (as prim)":T_Bool:134 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:131,"validB (as prim)":T_Bool:132,DLC_Bool False);
            const "outcome (as prim)":T_Bool:136 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:129,"validB (as prim)":T_Bool:134,DLC_Bool False);
            if "outcome (as prim)":T_Bool:136 then {
              const "outcome (as prim)":T_UInt256:137 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:77);
              const "outcome (as prim)":T_UInt256:138 PL_Once = ADD("_handA (as clo app)":T_UInt256:104,"outcome (as prim)":T_UInt256:137);
              const "outcome (as prim)":T_UInt256:139 PL_Once = MOD("outcome (as prim)":T_UInt256:138,DLC_Int 3);
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
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
                 };
               };
            let "clo app":T_Bytes:205;
            const "prim":T_Bool:207 PL_Once = PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
            const "prim":T_Bool:208 PL_Once = PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
            const "prim":T_Bool:210 PL_Once = IF_THEN_ELSE("prim":T_Bool:207,"prim":T_Bool:208,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:210);
            const "prim":T_Bool:211 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
            if "prim":T_Bool:211 then {
              "clo app":T_Bytes:205 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:212 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 1);
              if "prim":T_Bool:212 then {
                "clo app":T_Bytes:205 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:213 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
                if "prim":T_Bool:213 then {
                  "clo app":T_Bytes:205 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:214 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 3);
                  if "prim":T_Bool:214 then {
                    "clo app":T_Bytes:205 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:205 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            exit("clo app":T_Bytes:205); } } } } },
  "O" = interact {
    };
  sendrecv join("A":T_Address:7) 1 ()("array idx":T_UInt256:5, "array idx":T_UInt256:6){
    const "prim":T_UInt256:9 PL_Once = ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
    const "prim":T_UInt256:10 PL_Once = TXN_VALUE();
    const "prim":T_Bool:11 PL_Once = PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
    claim(CT_Require)("prim":T_Bool:11);
    claim(CT_Require)(DLC_Bool True);
    sendrecv join("B":T_Address:15) 2 ()().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:7) 9 ()(){
        const "prim":T_UInt256:23 PL_Once = TXN_VALUE();
        const "prim":T_Bool:24 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:23);
        claim(CT_Require)("prim":T_Bool:24);
        exit(DLC_Bytes "Bob quits"); } }){
      const "prim":T_UInt256:16 PL_Once = TXN_VALUE();
      const "prim":T_Bool:17 PL_Once = PEQ("array idx":T_UInt256:5,"prim":T_UInt256:16);
      claim(CT_Require)("prim":T_Bool:17);
      claim(CT_Require)(DLC_Bool True);
      sendrecv again("A":T_Address:7) 3 ()("commitment (as digest)":T_UInt256:48).timeout(DLC_Int 10, {
        sendrecv again("B":T_Address:15) 8 ()(){
          const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
          const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
          claim(CT_Require)("prim":T_Bool:57);
          exit(DLC_Bytes "Alice quits"); } }){
        const "prim":T_UInt256:49 PL_Once = TXN_VALUE();
        const "prim":T_Bool:50 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:49);
        claim(CT_Require)("prim":T_Bool:50);
        claim(CT_Require)(DLC_Bool True);
        sendrecv again("B":T_Address:15) 4 ()("handB (as clo app)":T_UInt256:77).timeout(DLC_Int 10, {
          sendrecv again("A":T_Address:7) 7 ()(){
            const "prim":T_UInt256:85 PL_Once = TXN_VALUE();
            const "prim":T_Bool:86 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:85);
            claim(CT_Require)("prim":T_Bool:86);
            exit(DLC_Bytes "Bob quits"); } }){
          const "prim":T_UInt256:78 PL_Once = TXN_VALUE();
          const "prim":T_Bool:79 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:78);
          claim(CT_Require)("prim":T_Bool:79);
          const "prim":T_Bool:89 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
          const "prim":T_Bool:90 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
          const "prim":T_Bool:92 PL_Once = IF_THEN_ELSE("prim":T_Bool:89,"prim":T_Bool:90,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:92);
          claim(CT_Require)(DLC_Bool True);
          sendrecv again("A":T_Address:7) 5 ()("salt (as interact)":T_UInt256:103, "_handA (as clo app)":T_UInt256:104).timeout(DLC_Int 10, {
            sendrecv again("B":T_Address:15) 6 ()(){
              const "prim":T_UInt256:112 PL_Once = TXN_VALUE();
              const "prim":T_Bool:113 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:112);
              claim(CT_Require)("prim":T_Bool:113);
              exit(DLC_Bytes "Alice quits"); } }){
            const "prim":T_UInt256:105 PL_Once = TXN_VALUE();
            const "prim":T_Bool:106 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:105);
            claim(CT_Require)("prim":T_Bool:106);
            const "digest":T_UInt256:116 PL_Once = digest("salt (as interact)":T_UInt256:103,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:117 PL_Once = PEQ("commitment (as digest)":T_UInt256:48,"digest":T_UInt256:116);
            claim(CT_Require)("prim":T_Bool:117);
            const "prim":T_Bool:119 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "prim":T_Bool:120 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "prim":T_Bool:122 PL_Once = IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:122);
            let "outcome (as clo app)":T_UInt256:124;
            const "validA (as prim)":T_Bool:126 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
            const "validA (as prim)":T_Bool:127 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
            const "validA (as prim)":T_Bool:129 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:126,"validA (as prim)":T_Bool:127,DLC_Bool False);
            const "validB (as prim)":T_Bool:131 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
            const "validB (as prim)":T_Bool:132 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
            const "validB (as prim)":T_Bool:134 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:131,"validB (as prim)":T_Bool:132,DLC_Bool False);
            const "outcome (as prim)":T_Bool:136 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:129,"validB (as prim)":T_Bool:134,DLC_Bool False);
            if "outcome (as prim)":T_Bool:136 then {
              const "outcome (as prim)":T_UInt256:137 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:77);
              const "outcome (as prim)":T_UInt256:138 PL_Once = ADD("_handA (as clo app)":T_UInt256:104,"outcome (as prim)":T_UInt256:137);
              const "outcome (as prim)":T_UInt256:139 PL_Once = MOD("outcome (as prim)":T_UInt256:138,DLC_Int 3);
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
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
                 };
               };
            let "clo app":T_Bytes:205;
            const "prim":T_Bool:207 PL_Once = PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:124);
            const "prim":T_Bool:208 PL_Once = PLT("outcome (as clo app)":T_UInt256:124,DLC_Int 5);
            const "prim":T_Bool:210 PL_Once = IF_THEN_ELSE("prim":T_Bool:207,"prim":T_Bool:208,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:210);
            const "prim":T_Bool:211 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
            if "prim":T_Bool:211 then {
              "clo app":T_Bytes:205 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:212 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 1);
              if "prim":T_Bool:212 then {
                "clo app":T_Bytes:205 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:213 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
                if "prim":T_Bool:213 then {
                  "clo app":T_Bytes:205 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:214 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 3);
                  if "prim":T_Bool:214 then {
                    "clo app":T_Bytes:205 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:205 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            exit("clo app":T_Bytes:205); } } } } }}

{
  1 = {
    join("A":T_Address:7),
    (between [] []),
    last = 0,
    [],
    ["array idx":T_UInt256:5, "array idx":T_UInt256:6],
    {
      const "prim":T_UInt256:9 PL_Once = ADD("array idx":T_UInt256:5,"array idx":T_UInt256:6);
      const "prim":T_UInt256:10 PL_Once = TXN_VALUE();
      const "prim":T_Bool:11 PL_Once = PEQ("prim":T_UInt256:9,"prim":T_UInt256:10);
      claim(CT_Require)("prim":T_Bool:11);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6 ]) } },
  2 = {
    join("B":T_Address:15),
    (between [] [DLC_Int 10]),
    last = 1,
    ["A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6],
    [],
    {
      const "prim":T_UInt256:16 PL_Once = TXN_VALUE();
      const "prim":T_Bool:17 PL_Once = PEQ("array idx":T_UInt256:5,"prim":T_UInt256:16);
      claim(CT_Require)("prim":T_Bool:17);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6
             , "B":T_Address:15 ]) } },
  3 = {
    again("A":T_Address:7),
    (between [] [DLC_Int 10]),
    last = 2,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15 ],
    ["commitment (as digest)":T_UInt256:48],
    {
      const "prim":T_UInt256:49 PL_Once = TXN_VALUE();
      const "prim":T_Bool:50 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:49);
      claim(CT_Require)("prim":T_Bool:50);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6
             , "B":T_Address:15
             , "commitment (as digest)":T_UInt256:48 ]) } },
  4 = {
    again("B":T_Address:15),
    (between [] [DLC_Int 10]),
    last = 3,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15
    , "commitment (as digest)":T_UInt256:48 ],
    ["handB (as clo app)":T_UInt256:77],
    {
      const "prim":T_UInt256:78 PL_Once = TXN_VALUE();
      const "prim":T_Bool:79 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:78);
      claim(CT_Require)("prim":T_Bool:79);
      const "prim":T_Bool:89 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
      const "prim":T_Bool:90 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
      const "prim":T_Bool:92 PL_Once = IF_THEN_ELSE("prim":T_Bool:89,"prim":T_Bool:90,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:92);
      (wait! [ "A":T_Address:7
             , "array idx":T_UInt256:5
             , "array idx":T_UInt256:6
             , "B":T_Address:15
             , "commitment (as digest)":T_UInt256:48
             , "handB (as clo app)":T_UInt256:77 ]) } },
  5 = {
    again("A":T_Address:7),
    (between [] [DLC_Int 10]),
    last = 4,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15
    , "commitment (as digest)":T_UInt256:48
    , "handB (as clo app)":T_UInt256:77 ],
    ["salt (as interact)":T_UInt256:103, "_handA (as clo app)":T_UInt256:104],
    {
      const "prim":T_UInt256:105 PL_Once = TXN_VALUE();
      const "prim":T_Bool:106 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:105);
      claim(CT_Require)("prim":T_Bool:106);
      const "digest":T_UInt256:116 PL_Once = digest("salt (as interact)":T_UInt256:103,"_handA (as clo app)":T_UInt256:104);
      const "prim":T_Bool:117 PL_Once = PEQ("commitment (as digest)":T_UInt256:48,"digest":T_UInt256:116);
      claim(CT_Require)("prim":T_Bool:117);
      const "prim":T_Bool:119 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
      const "prim":T_Bool:120 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
      const "prim":T_Bool:122 PL_Once = IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:122);
      let "outcome (as clo app)":T_UInt256:124;
      const "validA (as prim)":T_Bool:126 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:104);
      const "validA (as prim)":T_Bool:127 PL_Once = PLT("_handA (as clo app)":T_UInt256:104,DLC_Int 3);
      const "validA (as prim)":T_Bool:129 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:126,"validA (as prim)":T_Bool:127,DLC_Bool False);
      const "validB (as prim)":T_Bool:131 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:77);
      const "validB (as prim)":T_Bool:132 PL_Once = PLT("handB (as clo app)":T_UInt256:77,DLC_Int 3);
      const "validB (as prim)":T_Bool:134 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:131,"validB (as prim)":T_Bool:132,DLC_Bool False);
      const "outcome (as prim)":T_Bool:136 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:129,"validB (as prim)":T_Bool:134,DLC_Bool False);
      if "outcome (as prim)":T_Bool:136 then {
        const "outcome (as prim)":T_UInt256:137 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:77);
        const "outcome (as prim)":T_UInt256:138 PL_Once = ADD("_handA (as clo app)":T_UInt256:104,"outcome (as prim)":T_UInt256:137);
        const "outcome (as prim)":T_UInt256:139 PL_Once = MOD("outcome (as prim)":T_UInt256:138,DLC_Int 3);
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
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:198 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:199,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 PL_Once = PEQ("outcome (as clo app)":T_UInt256:124,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:200 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201 PL_Once = MUL(DLC_Int 2,"array idx":T_UInt256:5);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:201];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197 = ["array idx":T_UInt256:5,"array idx":T_UInt256:5];
           };
         };
      const "array idx":T_UInt256:202 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197[DLC_Int 0];
      const "array idx":T_UInt256:203 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Array [T_UInt256,T_UInt256]:197[DLC_Int 1];
      const "prim":T_UInt256:204 PL_Once = ADD("array idx":T_UInt256:6,"array idx":T_UInt256:202);
      transfer.("prim":T_UInt256:204).to("A":T_Address:7);
      transfer.("array idx":T_UInt256:203).to("B":T_Address:15);
      (halt! ) } },
  6 = {
    again("B":T_Address:15),
    (between [DLC_Int 10] []),
    last = 4,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15
    , "commitment (as digest)":T_UInt256:48
    , "handB (as clo app)":T_UInt256:77 ],
    [],
    {
      const "prim":T_UInt256:112 PL_Once = TXN_VALUE();
      const "prim":T_Bool:113 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:112);
      claim(CT_Require)("prim":T_Bool:113);
      const "prim":T_UInt256:114 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:114).to("B":T_Address:15);
      (halt! ) } },
  7 = {
    again("A":T_Address:7),
    (between [DLC_Int 10] []),
    last = 3,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15
    , "commitment (as digest)":T_UInt256:48 ],
    [],
    {
      const "prim":T_UInt256:85 PL_Once = TXN_VALUE();
      const "prim":T_Bool:86 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:85);
      claim(CT_Require)("prim":T_Bool:86);
      const "prim":T_UInt256:87 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:87).to("A":T_Address:7);
      (halt! ) } },
  8 = {
    again("B":T_Address:15),
    (between [DLC_Int 10] []),
    last = 2,
    [ "A":T_Address:7
    , "array idx":T_UInt256:5
    , "array idx":T_UInt256:6
    , "B":T_Address:15 ],
    [],
    {
      const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
      const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
      claim(CT_Require)("prim":T_Bool:57);
      const "prim":T_UInt256:58 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:58).to("B":T_Address:15);
      (halt! ) } },
  9 = {
    again("A":T_Address:7),
    (between [DLC_Int 10] []),
    last = 1,
    ["A":T_Address:7, "array idx":T_UInt256:5, "array idx":T_UInt256:6],
    [],
    {
      const "prim":T_UInt256:23 PL_Once = TXN_VALUE();
      const "prim":T_Bool:24 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:23);
      claim(CT_Require)("prim":T_Bool:24);
      const "prim":T_UInt256:25 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:25).to("A":T_Address:7);
      (halt! ) } }}