#lang pl
{
  "A" = interact {
    commits = T_Fun [] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Tuple [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    reveals = T_Fun [T_Bytes] T_Null};
  const "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Tuple [T_UInt256,T_UInt256]:1 PL_Many = interact("A")."getParams"();
  const "tuple idx":T_UInt256:2 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Tuple [T_UInt256,T_UInt256]:1[0];
  const "tuple idx":T_UInt256:3 PL_Many = "one of [\"wagerAmount\",\"escrowAmount\"] (as interact)":T_Tuple [T_UInt256,T_UInt256]:1[1];
  
  const "prim":T_UInt256:7 PL_Once = ADD("tuple idx":T_UInt256:2,"tuple idx":T_UInt256:3);
  
  sendrecv join("A":T_Address:6) 1 (.publish(("tuple idx":T_UInt256:2,"tuple idx":T_UInt256:3), "prim":T_UInt256:7, ()))("tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5){
    const "prim":T_UInt256:8 PL_Once = ADD("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    const "prim":T_UInt256:9 PL_Once = TXN_VALUE();
    const "prim":T_Bool:10 PL_Once = PEQ("prim":T_UInt256:8,"prim":T_UInt256:9);
    claim(CT_Require)("prim":T_Bool:10);
    sendrecv join("B":T_Address:14) 2 ()().timeout(DLC_Int 10, {
      
      sendrecv again("A":T_Address:6) 9 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5)))(){
        const "prim":T_UInt256:19 PL_Once = TXN_VALUE();
        const "prim":T_Bool:20 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:19);
        claim(CT_Require)("prim":T_Bool:20);
        claim(CT_Require)(DLC_Bool True);
        eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const "prim":T_UInt256:15 PL_Once = TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      eff interact("A")."partnerIs"("B":T_Address:14);
      let "_handA (as clo app)":T_UInt256:36;
      const "s (as interact)":T_Bytes:37 PL_Many = interact("A")."getHand"();
      const "rockP (as prim)":T_Bool:38 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:37,DLC_Bytes "ROCK");
      const "paperP (as prim)":T_Bool:39 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:37,DLC_Bytes "PAPER");
      const "scissorsP (as prim)":T_Bool:40 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:37,DLC_Bytes "SCISSORS");
      const "_handA (as prim)":T_Bool:42 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:38,DLC_Bool True,"paperP (as prim)":T_Bool:39);
      const "_handA (as prim)":T_Bool:44 PL_Once = IF_THEN_ELSE("_handA (as prim)":T_Bool:42,DLC_Bool True,"scissorsP (as prim)":T_Bool:40);
      claim(CT_Assume)("_handA (as prim)":T_Bool:44);
      if "rockP (as prim)":T_Bool:38 then {
        "_handA (as clo app)":T_UInt256:36 = DLC_Int 0;
         }
      else {
        if "paperP (as prim)":T_Bool:39 then {
          "_handA (as clo app)":T_UInt256:36 = DLC_Int 1;
           }
        else {
          "_handA (as clo app)":T_UInt256:36 = DLC_Int 2;
           };
         };
      const "salt (as interact)":T_UInt256:52 PL_Many = interact("A")."random"();
      const "commitment (as digest)":T_UInt256:53 PL_Once = digest("salt (as interact)":T_UInt256:52,"_handA (as clo app)":T_UInt256:36);
      eff interact("A")."commits"();
      
      
      sendrecv again("A":T_Address:6) 3 (.publish(("commitment (as digest)":T_UInt256:53), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14)))("commitment (as digest)":T_UInt256:55).timeout(DLC_Int 10, {
        sendrecv again("B":T_Address:14) 8 ()(){
          const "prim":T_UInt256:60 PL_Once = TXN_VALUE();
          const "prim":T_Bool:61 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:60);
          claim(CT_Require)("prim":T_Bool:61);
          claim(CT_Require)(DLC_Bool True);
          eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
          
           } }){
        const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
        const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
        claim(CT_Require)("prim":T_Bool:57);
        sendrecv again("B":T_Address:14) 4 ()("handB (as clo app)":T_UInt256:92).timeout(DLC_Int 10, {
          
          sendrecv again("A":T_Address:6) 7 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14, "commitment (as digest)":T_UInt256:55)))(){
            const "prim":T_UInt256:97 PL_Once = TXN_VALUE();
            const "prim":T_Bool:98 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:97);
            claim(CT_Require)("prim":T_Bool:98);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
            
             } }){
          const "prim":T_UInt256:93 PL_Once = TXN_VALUE();
          const "prim":T_Bool:94 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:93);
          claim(CT_Require)("prim":T_Bool:94);
          const "prim":T_Bool:112 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
          const "prim":T_Bool:113 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
          const "prim":T_Bool:115 PL_Once = IF_THEN_ELSE("prim":T_Bool:112,"prim":T_Bool:113,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:115);
          let "clo app":T_Bytes:117;
          const "prim":T_Bool:119 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
          const "prim":T_Bool:120 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
          const "prim":T_Bool:122 PL_Once = IF_THEN_ELSE("prim":T_Bool:119,"prim":T_Bool:120,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:122);
          const "prim":T_Bool:123 PL_Once = PEQ("handB (as clo app)":T_UInt256:92,DLC_Int 0);
          if "prim":T_Bool:123 then {
            "clo app":T_Bytes:117 = DLC_Bytes "ROCK";
             }
          else {
            const "prim":T_Bool:124 PL_Once = PEQ("handB (as clo app)":T_UInt256:92,DLC_Int 1);
            if "prim":T_Bool:124 then {
              "clo app":T_Bytes:117 = DLC_Bytes "PAPER";
               }
            else {
              "clo app":T_Bytes:117 = DLC_Bytes "SCISSORS";
               };
             };
          eff interact("A")."reveals"("clo app":T_Bytes:117);
          
          
          sendrecv again("A":T_Address:6) 5 (.publish(("salt (as interact)":T_UInt256:52,"_handA (as clo app)":T_UInt256:36), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14, "commitment (as digest)":T_UInt256:55, "handB (as clo app)":T_UInt256:92)))("salt (as interact)":T_UInt256:126, "_handA (as clo app)":T_UInt256:127).timeout(DLC_Int 10, {
            sendrecv again("B":T_Address:14) 6 ()(){
              const "prim":T_UInt256:132 PL_Once = TXN_VALUE();
              const "prim":T_Bool:133 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:132);
              claim(CT_Require)("prim":T_Bool:133);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
              
               } }){
            const "prim":T_UInt256:128 PL_Once = TXN_VALUE();
            const "prim":T_Bool:129 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:128);
            claim(CT_Require)("prim":T_Bool:129);
            const "digest":T_UInt256:147 PL_Once = digest("salt (as interact)":T_UInt256:126,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:148 PL_Once = PEQ("commitment (as digest)":T_UInt256:55,"digest":T_UInt256:147);
            claim(CT_Require)("prim":T_Bool:148);
            const "prim":T_Bool:150 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:151 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "prim":T_Bool:153 PL_Once = IF_THEN_ELSE("prim":T_Bool:150,"prim":T_Bool:151,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:153);
            let "outcome (as clo app)":T_UInt256:155;
            const "validA (as prim)":T_Bool:157 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "validA (as prim)":T_Bool:158 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "validA (as prim)":T_Bool:160 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:157,"validA (as prim)":T_Bool:158,DLC_Bool False);
            const "validB (as prim)":T_Bool:162 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
            const "validB (as prim)":T_Bool:163 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
            const "validB (as prim)":T_Bool:165 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:162,"validB (as prim)":T_Bool:163,DLC_Bool False);
            const "outcome (as prim)":T_Bool:167 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:160,"validB (as prim)":T_Bool:165,DLC_Bool False);
            if "outcome (as prim)":T_Bool:167 then {
              const "outcome (as prim)":T_UInt256:168 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:92);
              const "outcome (as prim)":T_UInt256:169 PL_Once = ADD("_handA (as clo app)":T_UInt256:127,"outcome (as prim)":T_UInt256:168);
              const "outcome (as prim)":T_UInt256:170 PL_Once = MOD("outcome (as prim)":T_UInt256:169,DLC_Int 3);
              "outcome (as clo app)":T_UInt256:155 = "outcome (as prim)":T_UInt256:170;
               }
            else {
              if "validA (as prim)":T_Bool:160 then {
                "outcome (as clo app)":T_UInt256:155 = DLC_Int 2;
                 }
              else {
                if "validB (as prim)":T_Bool:165 then {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 0;
                   }
                else {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 1;
                   };
                 };
               };
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
                 };
               };
            let "clo app":T_Bytes:239;
            const "prim":T_Bool:241 PL_Once = PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:155);
            const "prim":T_Bool:242 PL_Once = PLT("outcome (as clo app)":T_UInt256:155,DLC_Int 5);
            const "prim":T_Bool:244 PL_Once = IF_THEN_ELSE("prim":T_Bool:241,"prim":T_Bool:242,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:244);
            const "prim":T_Bool:245 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
            if "prim":T_Bool:245 then {
              "clo app":T_Bytes:239 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:246 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 1);
              if "prim":T_Bool:246 then {
                "clo app":T_Bytes:239 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:247 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
                if "prim":T_Bool:247 then {
                  "clo app":T_Bytes:239 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:248 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 3);
                  if "prim":T_Bool:248 then {
                    "clo app":T_Bytes:239 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:239 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("A")."endsWith"("clo app":T_Bytes:239);
            
             } } } } },
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null};
  sendrecv join("A":T_Address:6) 1 ()("tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5){
    const "prim":T_UInt256:8 PL_Once = ADD("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    const "prim":T_UInt256:9 PL_Once = TXN_VALUE();
    const "prim":T_Bool:10 PL_Once = PEQ("prim":T_UInt256:8,"prim":T_UInt256:9);
    claim(CT_Require)("prim":T_Bool:10);
    eff interact("B")."partnerIs"("A":T_Address:6);
    eff interact("B")."acceptParams"("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    
    
    sendrecv join("B":T_Address:14) 2 (.publish((), "tuple idx":T_UInt256:4, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5)))().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:6) 9 ()(){
        const "prim":T_UInt256:19 PL_Once = TXN_VALUE();
        const "prim":T_Bool:20 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:19);
        claim(CT_Require)("prim":T_Bool:20);
        claim(CT_Require)(DLC_Bool True);
        eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const "prim":T_UInt256:15 PL_Once = TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      sendrecv again("A":T_Address:6) 3 ()("commitment (as digest)":T_UInt256:55).timeout(DLC_Int 10, {
        
        sendrecv again("B":T_Address:14) 8 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14)))(){
          const "prim":T_UInt256:60 PL_Once = TXN_VALUE();
          const "prim":T_Bool:61 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:60);
          claim(CT_Require)("prim":T_Bool:61);
          claim(CT_Require)(DLC_Bool True);
          eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
          
           } }){
        const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
        const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
        claim(CT_Require)("prim":T_Bool:57);
        let "handB (as clo app)":T_UInt256:76;
        const "s (as interact)":T_Bytes:77 PL_Many = interact("B")."getHand"();
        const "rockP (as prim)":T_Bool:78 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:77,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:79 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:77,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:80 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:77,DLC_Bytes "SCISSORS");
        const "handB (as prim)":T_Bool:82 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:78,DLC_Bool True,"paperP (as prim)":T_Bool:79);
        const "handB (as prim)":T_Bool:84 PL_Once = IF_THEN_ELSE("handB (as prim)":T_Bool:82,DLC_Bool True,"scissorsP (as prim)":T_Bool:80);
        claim(CT_Assume)("handB (as prim)":T_Bool:84);
        if "rockP (as prim)":T_Bool:78 then {
          "handB (as clo app)":T_UInt256:76 = DLC_Int 0;
           }
        else {
          if "paperP (as prim)":T_Bool:79 then {
            "handB (as clo app)":T_UInt256:76 = DLC_Int 1;
             }
          else {
            "handB (as clo app)":T_UInt256:76 = DLC_Int 2;
             };
           };
        eff interact("B")."shows"();
        
        
        sendrecv again("B":T_Address:14) 4 (.publish(("handB (as clo app)":T_UInt256:76), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14, "commitment (as digest)":T_UInt256:55)))("handB (as clo app)":T_UInt256:92).timeout(DLC_Int 10, {
          sendrecv again("A":T_Address:6) 7 ()(){
            const "prim":T_UInt256:97 PL_Once = TXN_VALUE();
            const "prim":T_Bool:98 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:97);
            claim(CT_Require)("prim":T_Bool:98);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
            
             } }){
          const "prim":T_UInt256:93 PL_Once = TXN_VALUE();
          const "prim":T_Bool:94 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:93);
          claim(CT_Require)("prim":T_Bool:94);
          const "prim":T_Bool:112 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
          const "prim":T_Bool:113 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
          const "prim":T_Bool:115 PL_Once = IF_THEN_ELSE("prim":T_Bool:112,"prim":T_Bool:113,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:115);
          sendrecv again("A":T_Address:6) 5 ()("salt (as interact)":T_UInt256:126, "_handA (as clo app)":T_UInt256:127).timeout(DLC_Int 10, {
            
            sendrecv again("B":T_Address:14) 6 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:14, "commitment (as digest)":T_UInt256:55, "handB (as clo app)":T_UInt256:92)))(){
              const "prim":T_UInt256:132 PL_Once = TXN_VALUE();
              const "prim":T_Bool:133 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:132);
              claim(CT_Require)("prim":T_Bool:133);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
              
               } }){
            const "prim":T_UInt256:128 PL_Once = TXN_VALUE();
            const "prim":T_Bool:129 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:128);
            claim(CT_Require)("prim":T_Bool:129);
            const "digest":T_UInt256:147 PL_Once = digest("salt (as interact)":T_UInt256:126,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:148 PL_Once = PEQ("commitment (as digest)":T_UInt256:55,"digest":T_UInt256:147);
            claim(CT_Require)("prim":T_Bool:148);
            const "prim":T_Bool:150 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:151 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "prim":T_Bool:153 PL_Once = IF_THEN_ELSE("prim":T_Bool:150,"prim":T_Bool:151,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:153);
            let "outcome (as clo app)":T_UInt256:155;
            const "validA (as prim)":T_Bool:157 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "validA (as prim)":T_Bool:158 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "validA (as prim)":T_Bool:160 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:157,"validA (as prim)":T_Bool:158,DLC_Bool False);
            const "validB (as prim)":T_Bool:162 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
            const "validB (as prim)":T_Bool:163 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
            const "validB (as prim)":T_Bool:165 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:162,"validB (as prim)":T_Bool:163,DLC_Bool False);
            const "outcome (as prim)":T_Bool:167 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:160,"validB (as prim)":T_Bool:165,DLC_Bool False);
            if "outcome (as prim)":T_Bool:167 then {
              const "outcome (as prim)":T_UInt256:168 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:92);
              const "outcome (as prim)":T_UInt256:169 PL_Once = ADD("_handA (as clo app)":T_UInt256:127,"outcome (as prim)":T_UInt256:168);
              const "outcome (as prim)":T_UInt256:170 PL_Once = MOD("outcome (as prim)":T_UInt256:169,DLC_Int 3);
              "outcome (as clo app)":T_UInt256:155 = "outcome (as prim)":T_UInt256:170;
               }
            else {
              if "validA (as prim)":T_Bool:160 then {
                "outcome (as clo app)":T_UInt256:155 = DLC_Int 2;
                 }
              else {
                if "validB (as prim)":T_Bool:165 then {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 0;
                   }
                else {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 1;
                   };
                 };
               };
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
                 };
               };
            let "clo app":T_Bytes:251;
            const "prim":T_Bool:253 PL_Once = PLE(DLC_Int 0,"outcome (as clo app)":T_UInt256:155);
            const "prim":T_Bool:254 PL_Once = PLT("outcome (as clo app)":T_UInt256:155,DLC_Int 5);
            const "prim":T_Bool:256 PL_Once = IF_THEN_ELSE("prim":T_Bool:253,"prim":T_Bool:254,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:256);
            const "prim":T_Bool:257 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
            if "prim":T_Bool:257 then {
              "clo app":T_Bytes:251 = DLC_Bytes "Bob wins";
               }
            else {
              const "prim":T_Bool:258 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 1);
              if "prim":T_Bool:258 then {
                "clo app":T_Bytes:251 = DLC_Bytes "Draw";
                 }
              else {
                const "prim":T_Bool:259 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
                if "prim":T_Bool:259 then {
                  "clo app":T_Bytes:251 = DLC_Bytes "Alice wins";
                   }
                else {
                  const "prim":T_Bool:260 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 3);
                  if "prim":T_Bool:260 then {
                    "clo app":T_Bytes:251 = DLC_Bytes "Alice quits";
                     }
                  else {
                    "clo app":T_Bytes:251 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("B")."endsWith"("clo app":T_Bytes:251);
            
             } } } } },
  "O" = interact {
    };
  sendrecv join("A":T_Address:6) 1 ()("tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5){
    const "prim":T_UInt256:8 PL_Once = ADD("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    const "prim":T_UInt256:9 PL_Once = TXN_VALUE();
    const "prim":T_Bool:10 PL_Once = PEQ("prim":T_UInt256:8,"prim":T_UInt256:9);
    claim(CT_Require)("prim":T_Bool:10);
    sendrecv join("B":T_Address:14) 2 ()().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:6) 9 ()(){
        const "prim":T_UInt256:19 PL_Once = TXN_VALUE();
        const "prim":T_Bool:20 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:19);
        claim(CT_Require)("prim":T_Bool:20);
         } }){
      const "prim":T_UInt256:15 PL_Once = TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      sendrecv again("A":T_Address:6) 3 ()("commitment (as digest)":T_UInt256:55).timeout(DLC_Int 10, {
        sendrecv again("B":T_Address:14) 8 ()(){
          const "prim":T_UInt256:60 PL_Once = TXN_VALUE();
          const "prim":T_Bool:61 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:60);
          claim(CT_Require)("prim":T_Bool:61);
           } }){
        const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
        const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
        claim(CT_Require)("prim":T_Bool:57);
        sendrecv again("B":T_Address:14) 4 ()("handB (as clo app)":T_UInt256:92).timeout(DLC_Int 10, {
          sendrecv again("A":T_Address:6) 7 ()(){
            const "prim":T_UInt256:97 PL_Once = TXN_VALUE();
            const "prim":T_Bool:98 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:97);
            claim(CT_Require)("prim":T_Bool:98);
             } }){
          const "prim":T_UInt256:93 PL_Once = TXN_VALUE();
          const "prim":T_Bool:94 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:93);
          claim(CT_Require)("prim":T_Bool:94);
          const "prim":T_Bool:112 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
          const "prim":T_Bool:113 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
          const "prim":T_Bool:115 PL_Once = IF_THEN_ELSE("prim":T_Bool:112,"prim":T_Bool:113,DLC_Bool False);
          claim(CT_Require)("prim":T_Bool:115);
          sendrecv again("A":T_Address:6) 5 ()("salt (as interact)":T_UInt256:126, "_handA (as clo app)":T_UInt256:127).timeout(DLC_Int 10, {
            sendrecv again("B":T_Address:14) 6 ()(){
              const "prim":T_UInt256:132 PL_Once = TXN_VALUE();
              const "prim":T_Bool:133 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:132);
              claim(CT_Require)("prim":T_Bool:133);
               } }){
            const "prim":T_UInt256:128 PL_Once = TXN_VALUE();
            const "prim":T_Bool:129 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:128);
            claim(CT_Require)("prim":T_Bool:129);
            const "digest":T_UInt256:147 PL_Once = digest("salt (as interact)":T_UInt256:126,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:148 PL_Once = PEQ("commitment (as digest)":T_UInt256:55,"digest":T_UInt256:147);
            claim(CT_Require)("prim":T_Bool:148);
            const "prim":T_Bool:150 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "prim":T_Bool:151 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "prim":T_Bool:153 PL_Once = IF_THEN_ELSE("prim":T_Bool:150,"prim":T_Bool:151,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:153);
            let "outcome (as clo app)":T_UInt256:155;
            const "validA (as prim)":T_Bool:157 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
            const "validA (as prim)":T_Bool:158 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
            const "validA (as prim)":T_Bool:160 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:157,"validA (as prim)":T_Bool:158,DLC_Bool False);
            const "validB (as prim)":T_Bool:162 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
            const "validB (as prim)":T_Bool:163 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
            const "validB (as prim)":T_Bool:165 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:162,"validB (as prim)":T_Bool:163,DLC_Bool False);
            const "outcome (as prim)":T_Bool:167 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:160,"validB (as prim)":T_Bool:165,DLC_Bool False);
            if "outcome (as prim)":T_Bool:167 then {
              const "outcome (as prim)":T_UInt256:168 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:92);
              const "outcome (as prim)":T_UInt256:169 PL_Once = ADD("_handA (as clo app)":T_UInt256:127,"outcome (as prim)":T_UInt256:168);
              const "outcome (as prim)":T_UInt256:170 PL_Once = MOD("outcome (as prim)":T_UInt256:169,DLC_Int 3);
              "outcome (as clo app)":T_UInt256:155 = "outcome (as prim)":T_UInt256:170;
               }
            else {
              if "validA (as prim)":T_Bool:160 then {
                "outcome (as clo app)":T_UInt256:155 = DLC_Int 2;
                 }
              else {
                if "validB (as prim)":T_Bool:165 then {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 0;
                   }
                else {
                  "outcome (as clo app)":T_UInt256:155 = DLC_Int 1;
                   };
                 };
               };
            let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228;
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
            if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
              "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230,DLC_Int 0];
               }
            else {
              const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
              if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 then {
                const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232];
                 }
              else {
                "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
                 };
               };
             } } } } }}

{
  1 = {
    join("A":T_Address:6),
    (between [] []),
    last = 0,
    [],
    ["tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5],
    {
      const "prim":T_UInt256:8 PL_Once = ADD("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
      const "prim":T_UInt256:9 PL_Once = TXN_VALUE();
      const "prim":T_Bool:10 PL_Once = PEQ("prim":T_UInt256:8,"prim":T_UInt256:9);
      claim(CT_Require)("prim":T_Bool:10);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5 ]) } },
  2 = {
    join("B":T_Address:14),
    (between [] [DLC_Int 10]),
    last = 1,
    ["A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5],
    [],
    {
      const "prim":T_UInt256:15 PL_Once = TXN_VALUE();
      const "prim":T_Bool:16 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:15);
      claim(CT_Require)("prim":T_Bool:16);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5
             , "B":T_Address:14 ]) } },
  3 = {
    again("A":T_Address:6),
    (between [] [DLC_Int 10]),
    last = 2,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14 ],
    ["commitment (as digest)":T_UInt256:55],
    {
      const "prim":T_UInt256:56 PL_Once = TXN_VALUE();
      const "prim":T_Bool:57 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:56);
      claim(CT_Require)("prim":T_Bool:57);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5
             , "B":T_Address:14
             , "commitment (as digest)":T_UInt256:55 ]) } },
  4 = {
    again("B":T_Address:14),
    (between [] [DLC_Int 10]),
    last = 3,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:55 ],
    ["handB (as clo app)":T_UInt256:92],
    {
      const "prim":T_UInt256:93 PL_Once = TXN_VALUE();
      const "prim":T_Bool:94 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:93);
      claim(CT_Require)("prim":T_Bool:94);
      const "prim":T_Bool:112 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
      const "prim":T_Bool:113 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
      const "prim":T_Bool:115 PL_Once = IF_THEN_ELSE("prim":T_Bool:112,"prim":T_Bool:113,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:115);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5
             , "B":T_Address:14
             , "commitment (as digest)":T_UInt256:55
             , "handB (as clo app)":T_UInt256:92 ]) } },
  5 = {
    again("A":T_Address:6),
    (between [] [DLC_Int 10]),
    last = 4,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:55
    , "handB (as clo app)":T_UInt256:92 ],
    ["salt (as interact)":T_UInt256:126, "_handA (as clo app)":T_UInt256:127],
    {
      const "prim":T_UInt256:128 PL_Once = TXN_VALUE();
      const "prim":T_Bool:129 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:128);
      claim(CT_Require)("prim":T_Bool:129);
      const "digest":T_UInt256:147 PL_Once = digest("salt (as interact)":T_UInt256:126,"_handA (as clo app)":T_UInt256:127);
      const "prim":T_Bool:148 PL_Once = PEQ("commitment (as digest)":T_UInt256:55,"digest":T_UInt256:147);
      claim(CT_Require)("prim":T_Bool:148);
      const "prim":T_Bool:150 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
      const "prim":T_Bool:151 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
      const "prim":T_Bool:153 PL_Once = IF_THEN_ELSE("prim":T_Bool:150,"prim":T_Bool:151,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:153);
      let "outcome (as clo app)":T_UInt256:155;
      const "validA (as prim)":T_Bool:157 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:127);
      const "validA (as prim)":T_Bool:158 PL_Once = PLT("_handA (as clo app)":T_UInt256:127,DLC_Int 3);
      const "validA (as prim)":T_Bool:160 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:157,"validA (as prim)":T_Bool:158,DLC_Bool False);
      const "validB (as prim)":T_Bool:162 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:92);
      const "validB (as prim)":T_Bool:163 PL_Once = PLT("handB (as clo app)":T_UInt256:92,DLC_Int 3);
      const "validB (as prim)":T_Bool:165 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:162,"validB (as prim)":T_Bool:163,DLC_Bool False);
      const "outcome (as prim)":T_Bool:167 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:160,"validB (as prim)":T_Bool:165,DLC_Bool False);
      if "outcome (as prim)":T_Bool:167 then {
        const "outcome (as prim)":T_UInt256:168 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:92);
        const "outcome (as prim)":T_UInt256:169 PL_Once = ADD("_handA (as clo app)":T_UInt256:127,"outcome (as prim)":T_UInt256:168);
        const "outcome (as prim)":T_UInt256:170 PL_Once = MOD("outcome (as prim)":T_UInt256:169,DLC_Int 3);
        "outcome (as clo app)":T_UInt256:155 = "outcome (as prim)":T_UInt256:170;
         }
      else {
        if "validA (as prim)":T_Bool:160 then {
          "outcome (as clo app)":T_UInt256:155 = DLC_Int 2;
           }
        else {
          if "validB (as prim)":T_Bool:165 then {
            "outcome (as clo app)":T_UInt256:155 = DLC_Int 0;
             }
          else {
            "outcome (as clo app)":T_UInt256:155 = DLC_Int 1;
             };
           };
         };
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:229 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:230,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 PL_Once = PEQ("outcome (as clo app)":T_UInt256:155,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:231 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:232];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
           };
         };
      const "tuple idx":T_UInt256:233 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228[0];
      const "tuple idx":T_UInt256:234 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:228[1];
      const "prim":T_UInt256:235 PL_Once = ADD("tuple idx":T_UInt256:5,"tuple idx":T_UInt256:233);
      transfer.("prim":T_UInt256:235).to("A":T_Address:6);
      transfer.("tuple idx":T_UInt256:234).to("B":T_Address:14);
      (halt! ) } },
  6 = {
    again("B":T_Address:14),
    (between [DLC_Int 10] []),
    last = 4,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:55
    , "handB (as clo app)":T_UInt256:92 ],
    [],
    {
      const "prim":T_UInt256:132 PL_Once = TXN_VALUE();
      const "prim":T_Bool:133 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:132);
      claim(CT_Require)("prim":T_Bool:133);
      const "prim":T_UInt256:134 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:134).to("B":T_Address:14);
      (halt! ) } },
  7 = {
    again("A":T_Address:6),
    (between [DLC_Int 10] []),
    last = 3,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14
    , "commitment (as digest)":T_UInt256:55 ],
    [],
    {
      const "prim":T_UInt256:97 PL_Once = TXN_VALUE();
      const "prim":T_Bool:98 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:97);
      claim(CT_Require)("prim":T_Bool:98);
      const "prim":T_UInt256:99 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:99).to("A":T_Address:6);
      (halt! ) } },
  8 = {
    again("B":T_Address:14),
    (between [DLC_Int 10] []),
    last = 2,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:14 ],
    [],
    {
      const "prim":T_UInt256:60 PL_Once = TXN_VALUE();
      const "prim":T_Bool:61 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:60);
      claim(CT_Require)("prim":T_Bool:61);
      const "prim":T_UInt256:62 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:62).to("B":T_Address:14);
      (halt! ) } },
  9 = {
    again("A":T_Address:6),
    (between [DLC_Int 10] []),
    last = 1,
    ["A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5],
    [],
    {
      const "prim":T_UInt256:19 PL_Once = TXN_VALUE();
      const "prim":T_Bool:20 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:19);
      claim(CT_Require)("prim":T_Bool:20);
      const "prim":T_UInt256:21 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:21).to("A":T_Address:6);
      (halt! ) } }}