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
    sendrecv join("B":T_Address:13) 2 ()().timeout(DLC_Int 10, {
      
      sendrecv again("A":T_Address:6) 10 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5)))(){
        const "prim":T_UInt256:18 PL_Once = TXN_VALUE();
        const "prim":T_Bool:19 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:18);
        claim(CT_Require)("prim":T_Bool:19);
        claim(CT_Require)(DLC_Bool True);
        eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const "prim":T_UInt256:14 PL_Once = TXN_VALUE();
      const "prim":T_Bool:15 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:14);
      claim(CT_Require)("prim":T_Bool:15);
      loopvar {
        "count":T_UInt256:32 = DLC_Int 0,
        "outcome":T_UInt256:33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:57 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
         "prim":T_Bool:57) }
      {
        let "_handA (as clo app)":T_UInt256:60;
        const "s (as interact)":T_Bytes:61 PL_Many = interact("A")."getHand"();
        const "rockP (as prim)":T_Bool:62 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:61,DLC_Bytes "ROCK");
        const "paperP (as prim)":T_Bool:63 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:61,DLC_Bytes "PAPER");
        const "scissorsP (as prim)":T_Bool:64 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:61,DLC_Bytes "SCISSORS");
        const "_handA (as prim)":T_Bool:66 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:62,DLC_Bool True,"paperP (as prim)":T_Bool:63);
        const "_handA (as prim)":T_Bool:68 PL_Once = IF_THEN_ELSE("_handA (as prim)":T_Bool:66,DLC_Bool True,"scissorsP (as prim)":T_Bool:64);
        claim(CT_Assume)("_handA (as prim)":T_Bool:68);
        if "rockP (as prim)":T_Bool:62 then {
          "_handA (as clo app)":T_UInt256:60 = DLC_Int 0;
           }
        else {
          if "paperP (as prim)":T_Bool:63 then {
            "_handA (as clo app)":T_UInt256:60 = DLC_Int 1;
             }
          else {
            "_handA (as clo app)":T_UInt256:60 = DLC_Int 2;
             };
           };
        const "salt (as interact)":T_UInt256:76 PL_Many = interact("A")."random"();
        const "commitment (as digest)":T_UInt256:77 PL_Once = digest("salt (as interact)":T_UInt256:76,"_handA (as clo app)":T_UInt256:60);
        eff interact("A")."commits"();
        
        
        sendrecv again("A":T_Address:6) 4 (.publish(("commitment (as digest)":T_UInt256:77), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "count":T_UInt256:32)))("commitment (as digest)":T_UInt256:79).timeout(DLC_Int 10, {
          sendrecv again("B":T_Address:13) 9 ()(){
            const "prim":T_UInt256:84 PL_Once = TXN_VALUE();
            const "prim":T_Bool:85 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:84);
            claim(CT_Require)("prim":T_Bool:85);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
          const "prim":T_UInt256:80 PL_Once = TXN_VALUE();
          const "prim":T_Bool:81 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:80);
          claim(CT_Require)("prim":T_Bool:81);
          sendrecv again("B":T_Address:13) 5 ()("handB (as clo app)":T_UInt256:116).timeout(DLC_Int 10, {
            
            sendrecv again("A":T_Address:6) 8 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "commitment (as digest)":T_UInt256:79, "count":T_UInt256:32)))(){
              const "prim":T_UInt256:121 PL_Once = TXN_VALUE();
              const "prim":T_Bool:122 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:121);
              claim(CT_Require)("prim":T_Bool:122);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
            const "prim":T_UInt256:117 PL_Once = TXN_VALUE();
            const "prim":T_Bool:118 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:117);
            claim(CT_Require)("prim":T_Bool:118);
            const "prim":T_Bool:136 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
            const "prim":T_Bool:137 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
            const "prim":T_Bool:139 PL_Once = IF_THEN_ELSE("prim":T_Bool:136,"prim":T_Bool:137,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:139);
            let "clo app":T_Bytes:141;
            const "prim":T_Bool:143 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
            const "prim":T_Bool:144 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
            const "prim":T_Bool:146 PL_Once = IF_THEN_ELSE("prim":T_Bool:143,"prim":T_Bool:144,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:146);
            const "prim":T_Bool:147 PL_Once = PEQ("handB (as clo app)":T_UInt256:116,DLC_Int 0);
            if "prim":T_Bool:147 then {
              "clo app":T_Bytes:141 = DLC_Bytes "ROCK";
               }
            else {
              const "prim":T_Bool:148 PL_Once = PEQ("handB (as clo app)":T_UInt256:116,DLC_Int 1);
              if "prim":T_Bool:148 then {
                "clo app":T_Bytes:141 = DLC_Bytes "PAPER";
                 }
              else {
                "clo app":T_Bytes:141 = DLC_Bytes "SCISSORS";
                 };
               };
            eff interact("A")."reveals"("clo app":T_Bytes:141);
            
            
            sendrecv again("A":T_Address:6) 6 (.publish(("salt (as interact)":T_UInt256:76,"_handA (as clo app)":T_UInt256:60), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "commitment (as digest)":T_UInt256:79, "handB (as clo app)":T_UInt256:116, "count":T_UInt256:32)))("salt (as interact)":T_UInt256:150, "_handA (as clo app)":T_UInt256:151).timeout(DLC_Int 10, {
              sendrecv again("B":T_Address:13) 7 ()(){
                const "prim":T_UInt256:156 PL_Once = TXN_VALUE();
                const "prim":T_Bool:157 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:156);
                claim(CT_Require)("prim":T_Bool:157);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
              const "prim":T_UInt256:152 PL_Once = TXN_VALUE();
              const "prim":T_Bool:153 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:152);
              claim(CT_Require)("prim":T_Bool:153);
              const "digest":T_UInt256:171 PL_Once = digest("salt (as interact)":T_UInt256:150,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:172 PL_Once = PEQ("commitment (as digest)":T_UInt256:79,"digest":T_UInt256:171);
              claim(CT_Require)("prim":T_Bool:172);
              const "prim":T_Bool:174 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:175 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "prim":T_Bool:177 PL_Once = IF_THEN_ELSE("prim":T_Bool:174,"prim":T_Bool:175,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:177);
              let "roundOutcome (as clo app)":T_UInt256:179;
              const "validA (as prim)":T_Bool:181 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "validA (as prim)":T_Bool:182 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "validA (as prim)":T_Bool:184 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:181,"validA (as prim)":T_Bool:182,DLC_Bool False);
              const "validB (as prim)":T_Bool:186 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
              const "validB (as prim)":T_Bool:187 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
              const "validB (as prim)":T_Bool:189 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:186,"validB (as prim)":T_Bool:187,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:191 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:184,"validB (as prim)":T_Bool:189,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:191 then {
                const "roundOutcome (as prim)":T_UInt256:192 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:116);
                const "roundOutcome (as prim)":T_UInt256:193 PL_Once = ADD("_handA (as clo app)":T_UInt256:151,"roundOutcome (as prim)":T_UInt256:192);
                const "roundOutcome (as prim)":T_UInt256:194 PL_Once = MOD("roundOutcome (as prim)":T_UInt256:193,DLC_Int 3);
                "roundOutcome (as clo app)":T_UInt256:179 = "roundOutcome (as prim)":T_UInt256:194;
                 }
              else {
                if "validA (as prim)":T_Bool:184 then {
                  "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 2;
                   }
                else {
                  if "validB (as prim)":T_Bool:189 then {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 0;
                     }
                  else {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 1;
                     };
                   };
                 };
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252 PL_Once = ADD(DLC_Int 1,"count":T_UInt256:32);
              {
                "count":T_UInt256:32 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252,
                "outcome":T_UInt256:33 = "roundOutcome (as clo app)":T_UInt256:179}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
           };
         };
      let "clo app":T_Bytes:268;
      const "prim":T_Bool:270 PL_Once = PLE(DLC_Int 0,"outcome":T_UInt256:33);
      const "prim":T_Bool:271 PL_Once = PLT("outcome":T_UInt256:33,DLC_Int 5);
      const "prim":T_Bool:273 PL_Once = IF_THEN_ELSE("prim":T_Bool:270,"prim":T_Bool:271,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:273);
      const "prim":T_Bool:274 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
      if "prim":T_Bool:274 then {
        "clo app":T_Bytes:268 = DLC_Bytes "Bob wins";
         }
      else {
        const "prim":T_Bool:275 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
        if "prim":T_Bool:275 then {
          "clo app":T_Bytes:268 = DLC_Bytes "Draw";
           }
        else {
          const "prim":T_Bool:276 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
          if "prim":T_Bool:276 then {
            "clo app":T_Bytes:268 = DLC_Bytes "Alice wins";
             }
          else {
            const "prim":T_Bool:277 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 3);
            if "prim":T_Bool:277 then {
              "clo app":T_Bytes:268 = DLC_Bytes "Alice quits";
               }
            else {
              "clo app":T_Bytes:268 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      eff interact("A")."endsWith"("clo app":T_Bytes:268);
      
       } },
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
    eff interact("B")."acceptParams"("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    
    
    sendrecv join("B":T_Address:13) 2 (.publish((), "tuple idx":T_UInt256:4, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5)))().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:6) 10 ()(){
        const "prim":T_UInt256:18 PL_Once = TXN_VALUE();
        const "prim":T_Bool:19 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:18);
        claim(CT_Require)("prim":T_Bool:19);
        claim(CT_Require)(DLC_Bool True);
        eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const "prim":T_UInt256:14 PL_Once = TXN_VALUE();
      const "prim":T_Bool:15 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:14);
      claim(CT_Require)("prim":T_Bool:15);
      loopvar {
        "count":T_UInt256:32 = DLC_Int 0,
        "outcome":T_UInt256:33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:57 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
         "prim":T_Bool:57) }
      {
        sendrecv again("A":T_Address:6) 4 ()("commitment (as digest)":T_UInt256:79).timeout(DLC_Int 10, {
          
          sendrecv again("B":T_Address:13) 9 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "count":T_UInt256:32)))(){
            const "prim":T_UInt256:84 PL_Once = TXN_VALUE();
            const "prim":T_Bool:85 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:84);
            claim(CT_Require)("prim":T_Bool:85);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
          const "prim":T_UInt256:80 PL_Once = TXN_VALUE();
          const "prim":T_Bool:81 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:80);
          claim(CT_Require)("prim":T_Bool:81);
          let "handB (as clo app)":T_UInt256:100;
          const "s (as interact)":T_Bytes:101 PL_Many = interact("B")."getHand"();
          const "rockP (as prim)":T_Bool:102 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:101,DLC_Bytes "ROCK");
          const "paperP (as prim)":T_Bool:103 PL_Many = BYTES_EQ("s (as interact)":T_Bytes:101,DLC_Bytes "PAPER");
          const "scissorsP (as prim)":T_Bool:104 PL_Once = BYTES_EQ("s (as interact)":T_Bytes:101,DLC_Bytes "SCISSORS");
          const "handB (as prim)":T_Bool:106 PL_Once = IF_THEN_ELSE("rockP (as prim)":T_Bool:102,DLC_Bool True,"paperP (as prim)":T_Bool:103);
          const "handB (as prim)":T_Bool:108 PL_Once = IF_THEN_ELSE("handB (as prim)":T_Bool:106,DLC_Bool True,"scissorsP (as prim)":T_Bool:104);
          claim(CT_Assume)("handB (as prim)":T_Bool:108);
          if "rockP (as prim)":T_Bool:102 then {
            "handB (as clo app)":T_UInt256:100 = DLC_Int 0;
             }
          else {
            if "paperP (as prim)":T_Bool:103 then {
              "handB (as clo app)":T_UInt256:100 = DLC_Int 1;
               }
            else {
              "handB (as clo app)":T_UInt256:100 = DLC_Int 2;
               };
             };
          eff interact("B")."shows"();
          
          
          sendrecv again("B":T_Address:13) 5 (.publish(("handB (as clo app)":T_UInt256:100), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "commitment (as digest)":T_UInt256:79, "count":T_UInt256:32)))("handB (as clo app)":T_UInt256:116).timeout(DLC_Int 10, {
            sendrecv again("A":T_Address:6) 8 ()(){
              const "prim":T_UInt256:121 PL_Once = TXN_VALUE();
              const "prim":T_Bool:122 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:121);
              claim(CT_Require)("prim":T_Bool:122);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
            const "prim":T_UInt256:117 PL_Once = TXN_VALUE();
            const "prim":T_Bool:118 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:117);
            claim(CT_Require)("prim":T_Bool:118);
            const "prim":T_Bool:136 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
            const "prim":T_Bool:137 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
            const "prim":T_Bool:139 PL_Once = IF_THEN_ELSE("prim":T_Bool:136,"prim":T_Bool:137,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:139);
            sendrecv again("A":T_Address:6) 6 ()("salt (as interact)":T_UInt256:150, "_handA (as clo app)":T_UInt256:151).timeout(DLC_Int 10, {
              
              sendrecv again("B":T_Address:13) 7 (.publish((), DLC_Int 0, ("A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5, "B":T_Address:13, "commitment (as digest)":T_UInt256:79, "handB (as clo app)":T_UInt256:116, "count":T_UInt256:32)))(){
                const "prim":T_UInt256:156 PL_Once = TXN_VALUE();
                const "prim":T_Bool:157 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:156);
                claim(CT_Require)("prim":T_Bool:157);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
              const "prim":T_UInt256:152 PL_Once = TXN_VALUE();
              const "prim":T_Bool:153 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:152);
              claim(CT_Require)("prim":T_Bool:153);
              const "digest":T_UInt256:171 PL_Once = digest("salt (as interact)":T_UInt256:150,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:172 PL_Once = PEQ("commitment (as digest)":T_UInt256:79,"digest":T_UInt256:171);
              claim(CT_Require)("prim":T_Bool:172);
              const "prim":T_Bool:174 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:175 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "prim":T_Bool:177 PL_Once = IF_THEN_ELSE("prim":T_Bool:174,"prim":T_Bool:175,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:177);
              let "roundOutcome (as clo app)":T_UInt256:179;
              const "validA (as prim)":T_Bool:181 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "validA (as prim)":T_Bool:182 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "validA (as prim)":T_Bool:184 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:181,"validA (as prim)":T_Bool:182,DLC_Bool False);
              const "validB (as prim)":T_Bool:186 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
              const "validB (as prim)":T_Bool:187 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
              const "validB (as prim)":T_Bool:189 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:186,"validB (as prim)":T_Bool:187,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:191 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:184,"validB (as prim)":T_Bool:189,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:191 then {
                const "roundOutcome (as prim)":T_UInt256:192 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:116);
                const "roundOutcome (as prim)":T_UInt256:193 PL_Once = ADD("_handA (as clo app)":T_UInt256:151,"roundOutcome (as prim)":T_UInt256:192);
                const "roundOutcome (as prim)":T_UInt256:194 PL_Once = MOD("roundOutcome (as prim)":T_UInt256:193,DLC_Int 3);
                "roundOutcome (as clo app)":T_UInt256:179 = "roundOutcome (as prim)":T_UInt256:194;
                 }
              else {
                if "validA (as prim)":T_Bool:184 then {
                  "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 2;
                   }
                else {
                  if "validB (as prim)":T_Bool:189 then {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 0;
                     }
                  else {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 1;
                     };
                   };
                 };
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252 PL_Once = ADD(DLC_Int 1,"count":T_UInt256:32);
              {
                "count":T_UInt256:32 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252,
                "outcome":T_UInt256:33 = "roundOutcome (as clo app)":T_UInt256:179}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
           };
         };
      let "clo app":T_Bytes:280;
      const "prim":T_Bool:282 PL_Once = PLE(DLC_Int 0,"outcome":T_UInt256:33);
      const "prim":T_Bool:283 PL_Once = PLT("outcome":T_UInt256:33,DLC_Int 5);
      const "prim":T_Bool:285 PL_Once = IF_THEN_ELSE("prim":T_Bool:282,"prim":T_Bool:283,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:285);
      const "prim":T_Bool:286 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
      if "prim":T_Bool:286 then {
        "clo app":T_Bytes:280 = DLC_Bytes "Bob wins";
         }
      else {
        const "prim":T_Bool:287 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
        if "prim":T_Bool:287 then {
          "clo app":T_Bytes:280 = DLC_Bytes "Draw";
           }
        else {
          const "prim":T_Bool:288 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
          if "prim":T_Bool:288 then {
            "clo app":T_Bytes:280 = DLC_Bytes "Alice wins";
             }
          else {
            const "prim":T_Bool:289 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 3);
            if "prim":T_Bool:289 then {
              "clo app":T_Bytes:280 = DLC_Bytes "Alice quits";
               }
            else {
              "clo app":T_Bytes:280 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      eff interact("B")."endsWith"("clo app":T_Bytes:280);
      
       } },
  "O" = interact {
    };
  sendrecv join("A":T_Address:6) 1 ()("tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5){
    const "prim":T_UInt256:8 PL_Once = ADD("tuple idx":T_UInt256:4,"tuple idx":T_UInt256:5);
    const "prim":T_UInt256:9 PL_Once = TXN_VALUE();
    const "prim":T_Bool:10 PL_Once = PEQ("prim":T_UInt256:8,"prim":T_UInt256:9);
    claim(CT_Require)("prim":T_Bool:10);
    sendrecv join("B":T_Address:13) 2 ()().timeout(DLC_Int 10, {
      sendrecv again("A":T_Address:6) 10 ()(){
        const "prim":T_UInt256:18 PL_Once = TXN_VALUE();
        const "prim":T_Bool:19 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:18);
        claim(CT_Require)("prim":T_Bool:19);
         } }){
      const "prim":T_UInt256:14 PL_Once = TXN_VALUE();
      const "prim":T_Bool:15 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:14);
      claim(CT_Require)("prim":T_Bool:15);
      loopvar {
        "count":T_UInt256:32 = DLC_Int 0,
        "outcome":T_UInt256:33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const "prim":T_Bool:57 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
         "prim":T_Bool:57) }
      {
        sendrecv again("A":T_Address:6) 4 ()("commitment (as digest)":T_UInt256:79).timeout(DLC_Int 10, {
          sendrecv again("B":T_Address:13) 9 ()(){
            const "prim":T_UInt256:84 PL_Once = TXN_VALUE();
            const "prim":T_Bool:85 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:84);
            claim(CT_Require)("prim":T_Bool:85);
             } }){
          const "prim":T_UInt256:80 PL_Once = TXN_VALUE();
          const "prim":T_Bool:81 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:80);
          claim(CT_Require)("prim":T_Bool:81);
          sendrecv again("B":T_Address:13) 5 ()("handB (as clo app)":T_UInt256:116).timeout(DLC_Int 10, {
            sendrecv again("A":T_Address:6) 8 ()(){
              const "prim":T_UInt256:121 PL_Once = TXN_VALUE();
              const "prim":T_Bool:122 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:121);
              claim(CT_Require)("prim":T_Bool:122);
               } }){
            const "prim":T_UInt256:117 PL_Once = TXN_VALUE();
            const "prim":T_Bool:118 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:117);
            claim(CT_Require)("prim":T_Bool:118);
            const "prim":T_Bool:136 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
            const "prim":T_Bool:137 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
            const "prim":T_Bool:139 PL_Once = IF_THEN_ELSE("prim":T_Bool:136,"prim":T_Bool:137,DLC_Bool False);
            claim(CT_Require)("prim":T_Bool:139);
            sendrecv again("A":T_Address:6) 6 ()("salt (as interact)":T_UInt256:150, "_handA (as clo app)":T_UInt256:151).timeout(DLC_Int 10, {
              sendrecv again("B":T_Address:13) 7 ()(){
                const "prim":T_UInt256:156 PL_Once = TXN_VALUE();
                const "prim":T_Bool:157 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:156);
                claim(CT_Require)("prim":T_Bool:157);
                 } }){
              const "prim":T_UInt256:152 PL_Once = TXN_VALUE();
              const "prim":T_Bool:153 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:152);
              claim(CT_Require)("prim":T_Bool:153);
              const "digest":T_UInt256:171 PL_Once = digest("salt (as interact)":T_UInt256:150,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:172 PL_Once = PEQ("commitment (as digest)":T_UInt256:79,"digest":T_UInt256:171);
              claim(CT_Require)("prim":T_Bool:172);
              const "prim":T_Bool:174 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "prim":T_Bool:175 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "prim":T_Bool:177 PL_Once = IF_THEN_ELSE("prim":T_Bool:174,"prim":T_Bool:175,DLC_Bool False);
              claim(CT_Require)("prim":T_Bool:177);
              let "roundOutcome (as clo app)":T_UInt256:179;
              const "validA (as prim)":T_Bool:181 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
              const "validA (as prim)":T_Bool:182 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
              const "validA (as prim)":T_Bool:184 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:181,"validA (as prim)":T_Bool:182,DLC_Bool False);
              const "validB (as prim)":T_Bool:186 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
              const "validB (as prim)":T_Bool:187 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
              const "validB (as prim)":T_Bool:189 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:186,"validB (as prim)":T_Bool:187,DLC_Bool False);
              const "roundOutcome (as prim)":T_Bool:191 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:184,"validB (as prim)":T_Bool:189,DLC_Bool False);
              if "roundOutcome (as prim)":T_Bool:191 then {
                const "roundOutcome (as prim)":T_UInt256:192 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:116);
                const "roundOutcome (as prim)":T_UInt256:193 PL_Once = ADD("_handA (as clo app)":T_UInt256:151,"roundOutcome (as prim)":T_UInt256:192);
                const "roundOutcome (as prim)":T_UInt256:194 PL_Once = MOD("roundOutcome (as prim)":T_UInt256:193,DLC_Int 3);
                "roundOutcome (as clo app)":T_UInt256:179 = "roundOutcome (as prim)":T_UInt256:194;
                 }
              else {
                if "validA (as prim)":T_Bool:184 then {
                  "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 2;
                   }
                else {
                  if "validB (as prim)":T_Bool:189 then {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 0;
                     }
                  else {
                    "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 1;
                     };
                   };
                 };
              const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252 PL_Once = ADD(DLC_Int 1,"count":T_UInt256:32);
              {
                "count":T_UInt256:32 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252,
                "outcome":T_UInt256:33 = "roundOutcome (as clo app)":T_UInt256:179}
              continue; } } } }
      let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257;
      const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
      if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 then {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
        "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259,DLC_Int 0];
         }
      else {
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261];
           }
        else {
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
           };
         };
       } }}

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
    join("B":T_Address:13),
    (between [] [DLC_Int 10]),
    last = 1,
    ["A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5],
    [],
    {
      const "prim":T_UInt256:14 PL_Once = TXN_VALUE();
      const "prim":T_Bool:15 PL_Once = PEQ("tuple idx":T_UInt256:4,"prim":T_UInt256:14);
      claim(CT_Require)("prim":T_Bool:15);
      (jump! 3 [ "A":T_Address:6
               , "tuple idx":T_UInt256:4
               , "tuple idx":T_UInt256:5
               , "B":T_Address:13 ] {
        "count":T_UInt256:32 = DLC_Int 0,
        "outcome":T_UInt256:33 = DLC_Int 1}) } },
  3 = {
    loop!,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13 ],
    ["count":T_UInt256:32, "outcome":T_UInt256:33],
    {
      const "prim":T_Bool:57 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 1);
      
      if "prim":T_Bool:57 then {
        (wait! [ "A":T_Address:6
               , "tuple idx":T_UInt256:4
               , "tuple idx":T_UInt256:5
               , "B":T_Address:13
               , "count":T_UInt256:32 ]) }
      else {
        let "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257;
        const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 2);
        if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:258 then {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
          "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:259,DLC_Int 0];
           }
        else {
          const "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 PL_Once = PEQ("outcome":T_UInt256:33,DLC_Int 0);
          if "one of [\"getsA\",\"getsB\"] (as prim)":T_Bool:260 then {
            const "one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261 PL_Once = MUL(DLC_Int 2,"tuple idx":T_UInt256:4);
            "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = [DLC_Int 0,"one of [\"getsA\",\"getsB\"] (as prim)":T_UInt256:261];
             }
          else {
            "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257 = ["tuple idx":T_UInt256:4,"tuple idx":T_UInt256:4];
             };
           };
        const "tuple idx":T_UInt256:262 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257[0];
        const "tuple idx":T_UInt256:263 PL_Once = "one of [\"getsA\",\"getsB\"] (as clo app)":T_Tuple [T_UInt256,T_UInt256]:257[1];
        const "prim":T_UInt256:264 PL_Once = ADD("tuple idx":T_UInt256:5,"tuple idx":T_UInt256:262);
        transfer.("prim":T_UInt256:264).to("A":T_Address:6);
        transfer.("tuple idx":T_UInt256:263).to("B":T_Address:13);
        (halt! ) }; } },
  4 = {
    again("A":T_Address:6),
    (between [] [DLC_Int 10]),
    last = 3,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "count":T_UInt256:32 ],
    ["commitment (as digest)":T_UInt256:79],
    {
      const "prim":T_UInt256:80 PL_Once = TXN_VALUE();
      const "prim":T_Bool:81 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:80);
      claim(CT_Require)("prim":T_Bool:81);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5
             , "B":T_Address:13
             , "commitment (as digest)":T_UInt256:79
             , "count":T_UInt256:32 ]) } },
  5 = {
    again("B":T_Address:13),
    (between [] [DLC_Int 10]),
    last = 4,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "commitment (as digest)":T_UInt256:79
    , "count":T_UInt256:32 ],
    ["handB (as clo app)":T_UInt256:116],
    {
      const "prim":T_UInt256:117 PL_Once = TXN_VALUE();
      const "prim":T_Bool:118 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:117);
      claim(CT_Require)("prim":T_Bool:118);
      const "prim":T_Bool:136 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
      const "prim":T_Bool:137 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
      const "prim":T_Bool:139 PL_Once = IF_THEN_ELSE("prim":T_Bool:136,"prim":T_Bool:137,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:139);
      (wait! [ "A":T_Address:6
             , "tuple idx":T_UInt256:4
             , "tuple idx":T_UInt256:5
             , "B":T_Address:13
             , "commitment (as digest)":T_UInt256:79
             , "handB (as clo app)":T_UInt256:116
             , "count":T_UInt256:32 ]) } },
  6 = {
    again("A":T_Address:6),
    (between [] [DLC_Int 10]),
    last = 5,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "commitment (as digest)":T_UInt256:79
    , "handB (as clo app)":T_UInt256:116
    , "count":T_UInt256:32 ],
    ["salt (as interact)":T_UInt256:150, "_handA (as clo app)":T_UInt256:151],
    {
      const "prim":T_UInt256:152 PL_Once = TXN_VALUE();
      const "prim":T_Bool:153 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:152);
      claim(CT_Require)("prim":T_Bool:153);
      const "digest":T_UInt256:171 PL_Once = digest("salt (as interact)":T_UInt256:150,"_handA (as clo app)":T_UInt256:151);
      const "prim":T_Bool:172 PL_Once = PEQ("commitment (as digest)":T_UInt256:79,"digest":T_UInt256:171);
      claim(CT_Require)("prim":T_Bool:172);
      const "prim":T_Bool:174 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
      const "prim":T_Bool:175 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
      const "prim":T_Bool:177 PL_Once = IF_THEN_ELSE("prim":T_Bool:174,"prim":T_Bool:175,DLC_Bool False);
      claim(CT_Require)("prim":T_Bool:177);
      let "roundOutcome (as clo app)":T_UInt256:179;
      const "validA (as prim)":T_Bool:181 PL_Once = PLE(DLC_Int 0,"_handA (as clo app)":T_UInt256:151);
      const "validA (as prim)":T_Bool:182 PL_Once = PLT("_handA (as clo app)":T_UInt256:151,DLC_Int 3);
      const "validA (as prim)":T_Bool:184 PL_Many = IF_THEN_ELSE("validA (as prim)":T_Bool:181,"validA (as prim)":T_Bool:182,DLC_Bool False);
      const "validB (as prim)":T_Bool:186 PL_Once = PLE(DLC_Int 0,"handB (as clo app)":T_UInt256:116);
      const "validB (as prim)":T_Bool:187 PL_Once = PLT("handB (as clo app)":T_UInt256:116,DLC_Int 3);
      const "validB (as prim)":T_Bool:189 PL_Many = IF_THEN_ELSE("validB (as prim)":T_Bool:186,"validB (as prim)":T_Bool:187,DLC_Bool False);
      const "roundOutcome (as prim)":T_Bool:191 PL_Once = IF_THEN_ELSE("validA (as prim)":T_Bool:184,"validB (as prim)":T_Bool:189,DLC_Bool False);
      if "roundOutcome (as prim)":T_Bool:191 then {
        const "roundOutcome (as prim)":T_UInt256:192 PL_Once = SUB(DLC_Int 4,"handB (as clo app)":T_UInt256:116);
        const "roundOutcome (as prim)":T_UInt256:193 PL_Once = ADD("_handA (as clo app)":T_UInt256:151,"roundOutcome (as prim)":T_UInt256:192);
        const "roundOutcome (as prim)":T_UInt256:194 PL_Once = MOD("roundOutcome (as prim)":T_UInt256:193,DLC_Int 3);
        "roundOutcome (as clo app)":T_UInt256:179 = "roundOutcome (as prim)":T_UInt256:194;
         }
      else {
        if "validA (as prim)":T_Bool:184 then {
          "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 2;
           }
        else {
          if "validB (as prim)":T_Bool:189 then {
            "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 0;
             }
          else {
            "roundOutcome (as clo app)":T_UInt256:179 = DLC_Int 1;
             };
           };
         };
      const "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252 PL_Once = ADD(DLC_Int 1,"count":T_UInt256:32);
      (jump! 3 [ "A":T_Address:6
               , "tuple idx":T_UInt256:4
               , "tuple idx":T_UInt256:5
               , "B":T_Address:13 ] {
        "count":T_UInt256:32 = "one of [\"count\",\"outcome\"] (as prim)":T_UInt256:252,
        "outcome":T_UInt256:33 = "roundOutcome (as clo app)":T_UInt256:179}) } },
  7 = {
    again("B":T_Address:13),
    (between [DLC_Int 10] []),
    last = 5,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "commitment (as digest)":T_UInt256:79
    , "handB (as clo app)":T_UInt256:116
    , "count":T_UInt256:32 ],
    [],
    {
      const "prim":T_UInt256:156 PL_Once = TXN_VALUE();
      const "prim":T_Bool:157 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:156);
      claim(CT_Require)("prim":T_Bool:157);
      const "prim":T_UInt256:158 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:158).to("B":T_Address:13);
      (halt! ) } },
  8 = {
    again("A":T_Address:6),
    (between [DLC_Int 10] []),
    last = 4,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "commitment (as digest)":T_UInt256:79
    , "count":T_UInt256:32 ],
    [],
    {
      const "prim":T_UInt256:121 PL_Once = TXN_VALUE();
      const "prim":T_Bool:122 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:121);
      claim(CT_Require)("prim":T_Bool:122);
      const "prim":T_UInt256:123 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:123).to("A":T_Address:6);
      (halt! ) } },
  9 = {
    again("B":T_Address:13),
    (between [DLC_Int 10] []),
    last = 3,
    [ "A":T_Address:6
    , "tuple idx":T_UInt256:4
    , "tuple idx":T_UInt256:5
    , "B":T_Address:13
    , "count":T_UInt256:32 ],
    [],
    {
      const "prim":T_UInt256:84 PL_Once = TXN_VALUE();
      const "prim":T_Bool:85 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:84);
      claim(CT_Require)("prim":T_Bool:85);
      const "prim":T_UInt256:86 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:86).to("B":T_Address:13);
      (halt! ) } },
  10 = {
    again("A":T_Address:6),
    (between [DLC_Int 10] []),
    last = 1,
    ["A":T_Address:6, "tuple idx":T_UInt256:4, "tuple idx":T_UInt256:5],
    [],
    {
      const "prim":T_UInt256:18 PL_Once = TXN_VALUE();
      const "prim":T_Bool:19 PL_Once = PEQ(DLC_Int 0,"prim":T_UInt256:18);
      claim(CT_Require)("prim":T_Bool:19);
      const "prim":T_UInt256:20 PL_Once = BALANCE();
      transfer.("prim":T_UInt256:20).to("A":T_Address:6);
      (halt! ) } }}