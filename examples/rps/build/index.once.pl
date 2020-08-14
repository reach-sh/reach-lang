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
  const *v2 = interact("A")."getParams"();
  const *v3 = v2[0];
  const *v4 = v2[1];
  
  const !v8 = ADD(v3,v4);
  
  sendrecv join(v7) 1 (.publish((v3,v4), v8, ()))(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v11 = PEQ(v9,v10);
    claim(CT_Require)(v11);
    sendrecv join(v15) 2 ()()
      .timeout(DLC_Int 10, {
        
        sendrecv again(v7) 9 (.publish((), DLC_Int 0, (v7, v5, v6)))(){
          const !v20 = TXN_VALUE();
          const !v21 = PEQ(DLC_Int 0,v20);
          claim(CT_Require)(v21);
          claim(CT_Require)(DLC_Bool True);
          eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v16 = TXN_VALUE();
      const !v17 = PEQ(v5,v16);
      claim(CT_Require)(v17);
      eff interact("A")."partnerIs"(v15);
      
      let v39;
      const *v40 = interact("A")."getHand"();
      const *v41 = BYTES_EQ(v40,DLC_Bytes "ROCK");
      const *v42 = BYTES_EQ(v40,DLC_Bytes "PAPER");
      const !v43 = BYTES_EQ(v40,DLC_Bytes "SCISSORS");
      const !v45 = IF_THEN_ELSE(v41,DLC_Bool True,v42);
      const !v47 = IF_THEN_ELSE(v45,DLC_Bool True,v43);
      claim(CT_Assume)(v47);
      if v41 then {
        v39 = DLC_Int 0;
         }
      else {
        if v42 then {
          v39 = DLC_Int 1;
           }
        else {
          v39 = DLC_Int 2;
           };
         };
      const *v55 = interact("A")."random"();
      const !v56 = digest(v55,v39);
      eff interact("A")."commits"();
      
      
      sendrecv again(v7) 3 (.publish((v56), DLC_Int 0, (v7, v5, v6, v15)))(v58)
        .timeout(DLC_Int 10, {
          sendrecv again(v15) 8 ()(){
            const !v63 = TXN_VALUE();
            const !v64 = PEQ(DLC_Int 0,v63);
            claim(CT_Require)(v64);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v59 = TXN_VALUE();
        const !v60 = PEQ(DLC_Int 0,v59);
        claim(CT_Require)(v60);
        sendrecv again(v15) 4 ()(v95)
          .timeout(DLC_Int 10, {
            
            sendrecv again(v7) 7 (.publish((), DLC_Int 0, (v7, v5, v6, v15, v58)))(){
              const !v100 = TXN_VALUE();
              const !v101 = PEQ(DLC_Int 0,v100);
              claim(CT_Require)(v101);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v96 = TXN_VALUE();
          const !v97 = PEQ(DLC_Int 0,v96);
          claim(CT_Require)(v97);
          const !v115 = PLE(DLC_Int 0,v95);
          const !v116 = PLT(v95,DLC_Int 3);
          const !v118 = IF_THEN_ELSE(v115,v116,DLC_Bool False);
          claim(CT_Require)(v118);
          let v120;
          const !v122 = PLE(DLC_Int 0,v95);
          const !v123 = PLT(v95,DLC_Int 3);
          const !v125 = IF_THEN_ELSE(v122,v123,DLC_Bool False);
          claim(CT_Require)(v125);
          const !v126 = PEQ(v95,DLC_Int 0);
          if v126 then {
            v120 = DLC_Bytes "ROCK";
             }
          else {
            const !v127 = PEQ(v95,DLC_Int 1);
            if v127 then {
              v120 = DLC_Bytes "PAPER";
               }
            else {
              v120 = DLC_Bytes "SCISSORS";
               };
             };
          eff interact("A")."reveals"(v120);
          
          
          sendrecv again(v7) 5 (.publish((v55,v39), DLC_Int 0, (v7, v5, v6, v15, v58, v95)))(v129, v130)
            .timeout(DLC_Int 10, {
              sendrecv again(v15) 6 ()(){
                const !v135 = TXN_VALUE();
                const !v136 = PEQ(DLC_Int 0,v135);
                claim(CT_Require)(v136);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v131 = TXN_VALUE();
            const !v132 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v132);
            const !v150 = digest(v129,v130);
            const !v151 = PEQ(v58,v150);
            claim(CT_Require)(v151);
            const !v153 = PLE(DLC_Int 0,v130);
            const !v154 = PLT(v130,DLC_Int 3);
            const !v156 = IF_THEN_ELSE(v153,v154,DLC_Bool False);
            claim(CT_Require)(v156);
            let v158;
            const !v160 = PLE(DLC_Int 0,v130);
            const !v161 = PLT(v130,DLC_Int 3);
            const *v163 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
            const !v165 = PLE(DLC_Int 0,v95);
            const !v166 = PLT(v95,DLC_Int 3);
            const *v168 = IF_THEN_ELSE(v165,v166,DLC_Bool False);
            const !v170 = IF_THEN_ELSE(v163,v168,DLC_Bool False);
            if v170 then {
              const !v171 = SUB(DLC_Int 4,v95);
              const !v172 = ADD(v130,v171);
              const !v173 = MOD(v172,DLC_Int 3);
              v158 = v173;
               }
            else {
              if v163 then {
                v158 = DLC_Int 2;
                 }
              else {
                if v168 then {
                  v158 = DLC_Int 0;
                   }
                else {
                  v158 = DLC_Int 1;
                   };
                 };
               };
            let v232;
            const !v233 = PEQ(v158,DLC_Int 2);
            if v233 then {
              const !v234 = MUL(DLC_Int 2,v5);
              v232 = [v234,DLC_Int 0];
               }
            else {
              const !v235 = PEQ(v158,DLC_Int 0);
              if v235 then {
                const !v236 = MUL(DLC_Int 2,v5);
                v232 = [DLC_Int 0,v236];
                 }
              else {
                v232 = [v5,v5];
                 };
               };
            let v243;
            const !v245 = PLE(DLC_Int 0,v158);
            const !v246 = PLT(v158,DLC_Int 5);
            const !v248 = IF_THEN_ELSE(v245,v246,DLC_Bool False);
            claim(CT_Require)(v248);
            const !v249 = PEQ(v158,DLC_Int 0);
            if v249 then {
              v243 = DLC_Bytes "Bob wins";
               }
            else {
              const !v250 = PEQ(v158,DLC_Int 1);
              if v250 then {
                v243 = DLC_Bytes "Draw";
                 }
              else {
                const !v251 = PEQ(v158,DLC_Int 2);
                if v251 then {
                  v243 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v252 = PEQ(v158,DLC_Int 3);
                  if v252 then {
                    v243 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v243 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("A")."endsWith"(v243);
            
             } } } } },
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null};
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v11 = PEQ(v9,v10);
    claim(CT_Require)(v11);
    eff interact("B")."partnerIs"(v7);
    eff interact("B")."acceptParams"(v5,v6);
    
    
    sendrecv join(v15) 2 (.publish((), v5, (v7, v5, v6)))()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 9 ()(){
          const !v20 = TXN_VALUE();
          const !v21 = PEQ(DLC_Int 0,v20);
          claim(CT_Require)(v21);
          claim(CT_Require)(DLC_Bool True);
          eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v16 = TXN_VALUE();
      const !v17 = PEQ(v5,v16);
      claim(CT_Require)(v17);
      sendrecv again(v7) 3 ()(v58)
        .timeout(DLC_Int 10, {
          
          sendrecv again(v15) 8 (.publish((), DLC_Int 0, (v7, v5, v6, v15)))(){
            const !v63 = TXN_VALUE();
            const !v64 = PEQ(DLC_Int 0,v63);
            claim(CT_Require)(v64);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v59 = TXN_VALUE();
        const !v60 = PEQ(DLC_Int 0,v59);
        claim(CT_Require)(v60);
        let v79;
        const *v80 = interact("B")."getHand"();
        const *v81 = BYTES_EQ(v80,DLC_Bytes "ROCK");
        const *v82 = BYTES_EQ(v80,DLC_Bytes "PAPER");
        const !v83 = BYTES_EQ(v80,DLC_Bytes "SCISSORS");
        const !v85 = IF_THEN_ELSE(v81,DLC_Bool True,v82);
        const !v87 = IF_THEN_ELSE(v85,DLC_Bool True,v83);
        claim(CT_Assume)(v87);
        if v81 then {
          v79 = DLC_Int 0;
           }
        else {
          if v82 then {
            v79 = DLC_Int 1;
             }
          else {
            v79 = DLC_Int 2;
             };
           };
        eff interact("B")."shows"();
        
        
        sendrecv again(v15) 4 (.publish((v79), DLC_Int 0, (v7, v5, v6, v15, v58)))(v95)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v100 = TXN_VALUE();
              const !v101 = PEQ(DLC_Int 0,v100);
              claim(CT_Require)(v101);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v96 = TXN_VALUE();
          const !v97 = PEQ(DLC_Int 0,v96);
          claim(CT_Require)(v97);
          const !v115 = PLE(DLC_Int 0,v95);
          const !v116 = PLT(v95,DLC_Int 3);
          const !v118 = IF_THEN_ELSE(v115,v116,DLC_Bool False);
          claim(CT_Require)(v118);
          sendrecv again(v7) 5 ()(v129, v130)
            .timeout(DLC_Int 10, {
              
              sendrecv again(v15) 6 (.publish((), DLC_Int 0, (v7, v5, v6, v15, v58, v95)))(){
                const !v135 = TXN_VALUE();
                const !v136 = PEQ(DLC_Int 0,v135);
                claim(CT_Require)(v136);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v131 = TXN_VALUE();
            const !v132 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v132);
            const !v150 = digest(v129,v130);
            const !v151 = PEQ(v58,v150);
            claim(CT_Require)(v151);
            const !v153 = PLE(DLC_Int 0,v130);
            const !v154 = PLT(v130,DLC_Int 3);
            const !v156 = IF_THEN_ELSE(v153,v154,DLC_Bool False);
            claim(CT_Require)(v156);
            let v158;
            const !v160 = PLE(DLC_Int 0,v130);
            const !v161 = PLT(v130,DLC_Int 3);
            const *v163 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
            const !v165 = PLE(DLC_Int 0,v95);
            const !v166 = PLT(v95,DLC_Int 3);
            const *v168 = IF_THEN_ELSE(v165,v166,DLC_Bool False);
            const !v170 = IF_THEN_ELSE(v163,v168,DLC_Bool False);
            if v170 then {
              const !v171 = SUB(DLC_Int 4,v95);
              const !v172 = ADD(v130,v171);
              const !v173 = MOD(v172,DLC_Int 3);
              v158 = v173;
               }
            else {
              if v163 then {
                v158 = DLC_Int 2;
                 }
              else {
                if v168 then {
                  v158 = DLC_Int 0;
                   }
                else {
                  v158 = DLC_Int 1;
                   };
                 };
               };
            let v232;
            const !v233 = PEQ(v158,DLC_Int 2);
            if v233 then {
              const !v234 = MUL(DLC_Int 2,v5);
              v232 = [v234,DLC_Int 0];
               }
            else {
              const !v235 = PEQ(v158,DLC_Int 0);
              if v235 then {
                const !v236 = MUL(DLC_Int 2,v5);
                v232 = [DLC_Int 0,v236];
                 }
              else {
                v232 = [v5,v5];
                 };
               };
            let v255;
            const !v257 = PLE(DLC_Int 0,v158);
            const !v258 = PLT(v158,DLC_Int 5);
            const !v260 = IF_THEN_ELSE(v257,v258,DLC_Bool False);
            claim(CT_Require)(v260);
            const !v261 = PEQ(v158,DLC_Int 0);
            if v261 then {
              v255 = DLC_Bytes "Bob wins";
               }
            else {
              const !v262 = PEQ(v158,DLC_Int 1);
              if v262 then {
                v255 = DLC_Bytes "Draw";
                 }
              else {
                const !v263 = PEQ(v158,DLC_Int 2);
                if v263 then {
                  v255 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v264 = PEQ(v158,DLC_Int 3);
                  if v264 then {
                    v255 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v255 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("B")."endsWith"(v255);
            
             } } } } },
  "O" = interact {
    };
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v11 = PEQ(v9,v10);
    claim(CT_Require)(v11);
    sendrecv join(v15) 2 ()()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 9 ()(){
          const !v20 = TXN_VALUE();
          const !v21 = PEQ(DLC_Int 0,v20);
          claim(CT_Require)(v21);
           } }){
      const !v16 = TXN_VALUE();
      const !v17 = PEQ(v5,v16);
      claim(CT_Require)(v17);
      sendrecv again(v7) 3 ()(v58)
        .timeout(DLC_Int 10, {
          sendrecv again(v15) 8 ()(){
            const !v63 = TXN_VALUE();
            const !v64 = PEQ(DLC_Int 0,v63);
            claim(CT_Require)(v64);
             } }){
        const !v59 = TXN_VALUE();
        const !v60 = PEQ(DLC_Int 0,v59);
        claim(CT_Require)(v60);
        sendrecv again(v15) 4 ()(v95)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v100 = TXN_VALUE();
              const !v101 = PEQ(DLC_Int 0,v100);
              claim(CT_Require)(v101);
               } }){
          const !v96 = TXN_VALUE();
          const !v97 = PEQ(DLC_Int 0,v96);
          claim(CT_Require)(v97);
          const !v115 = PLE(DLC_Int 0,v95);
          const !v116 = PLT(v95,DLC_Int 3);
          const !v118 = IF_THEN_ELSE(v115,v116,DLC_Bool False);
          claim(CT_Require)(v118);
          sendrecv again(v7) 5 ()(v129, v130)
            .timeout(DLC_Int 10, {
              sendrecv again(v15) 6 ()(){
                const !v135 = TXN_VALUE();
                const !v136 = PEQ(DLC_Int 0,v135);
                claim(CT_Require)(v136);
                 } }){
            const !v131 = TXN_VALUE();
            const !v132 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v132);
            const !v150 = digest(v129,v130);
            const !v151 = PEQ(v58,v150);
            claim(CT_Require)(v151);
            const !v153 = PLE(DLC_Int 0,v130);
            const !v154 = PLT(v130,DLC_Int 3);
            const !v156 = IF_THEN_ELSE(v153,v154,DLC_Bool False);
            claim(CT_Require)(v156);
            let v158;
            const !v160 = PLE(DLC_Int 0,v130);
            const !v161 = PLT(v130,DLC_Int 3);
            const *v163 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
            const !v165 = PLE(DLC_Int 0,v95);
            const !v166 = PLT(v95,DLC_Int 3);
            const *v168 = IF_THEN_ELSE(v165,v166,DLC_Bool False);
            const !v170 = IF_THEN_ELSE(v163,v168,DLC_Bool False);
            if v170 then {
              const !v171 = SUB(DLC_Int 4,v95);
              const !v172 = ADD(v130,v171);
              const !v173 = MOD(v172,DLC_Int 3);
              v158 = v173;
               }
            else {
              if v163 then {
                v158 = DLC_Int 2;
                 }
              else {
                if v168 then {
                  v158 = DLC_Int 0;
                   }
                else {
                  v158 = DLC_Int 1;
                   };
                 };
               };
            let v232;
            const !v233 = PEQ(v158,DLC_Int 2);
            if v233 then {
              const !v234 = MUL(DLC_Int 2,v5);
              v232 = [v234,DLC_Int 0];
               }
            else {
              const !v235 = PEQ(v158,DLC_Int 0);
              if v235 then {
                const !v236 = MUL(DLC_Int 2,v5);
                v232 = [DLC_Int 0,v236];
                 }
              else {
                v232 = [v5,v5];
                 };
               };
             } } } } }}

{
  1 = {
    join(v7),
    (between [] []),
    last = 0,
    [],
    [v5, v6],
    {
      const !v9 = ADD(v5,v6);
      const !v10 = TXN_VALUE();
      const !v11 = PEQ(v9,v10);
      claim(CT_Require)(v11);
      (wait! [v7, v5, v6]) } },
  2 = {
    join(v15),
    (between [] [DLC_Int 10]),
    last = 1,
    [v7, v5, v6],
    [],
    {
      const !v16 = TXN_VALUE();
      const !v17 = PEQ(v5,v16);
      claim(CT_Require)(v17);
      (wait! [v7, v5, v6, v15]) } },
  3 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 2,
    [v7, v5, v6, v15],
    [v58],
    {
      const !v59 = TXN_VALUE();
      const !v60 = PEQ(DLC_Int 0,v59);
      claim(CT_Require)(v60);
      (wait! [v7, v5, v6, v15, v58]) } },
  4 = {
    again(v15),
    (between [] [DLC_Int 10]),
    last = 3,
    [v7, v5, v6, v15, v58],
    [v95],
    {
      const !v96 = TXN_VALUE();
      const !v97 = PEQ(DLC_Int 0,v96);
      claim(CT_Require)(v97);
      const !v115 = PLE(DLC_Int 0,v95);
      const !v116 = PLT(v95,DLC_Int 3);
      const !v118 = IF_THEN_ELSE(v115,v116,DLC_Bool False);
      claim(CT_Require)(v118);
      (wait! [v7, v5, v6, v15, v58, v95]) } },
  5 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 4,
    [v7, v5, v6, v15, v58, v95],
    [v129, v130],
    {
      const !v131 = TXN_VALUE();
      const !v132 = PEQ(DLC_Int 0,v131);
      claim(CT_Require)(v132);
      const !v150 = digest(v129,v130);
      const !v151 = PEQ(v58,v150);
      claim(CT_Require)(v151);
      const !v153 = PLE(DLC_Int 0,v130);
      const !v154 = PLT(v130,DLC_Int 3);
      const !v156 = IF_THEN_ELSE(v153,v154,DLC_Bool False);
      claim(CT_Require)(v156);
      let v158;
      const !v160 = PLE(DLC_Int 0,v130);
      const !v161 = PLT(v130,DLC_Int 3);
      const *v163 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
      const !v165 = PLE(DLC_Int 0,v95);
      const !v166 = PLT(v95,DLC_Int 3);
      const *v168 = IF_THEN_ELSE(v165,v166,DLC_Bool False);
      const !v170 = IF_THEN_ELSE(v163,v168,DLC_Bool False);
      if v170 then {
        const !v171 = SUB(DLC_Int 4,v95);
        const !v172 = ADD(v130,v171);
        const !v173 = MOD(v172,DLC_Int 3);
        v158 = v173;
         }
      else {
        if v163 then {
          v158 = DLC_Int 2;
           }
        else {
          if v168 then {
            v158 = DLC_Int 0;
             }
          else {
            v158 = DLC_Int 1;
             };
           };
         };
      let v232;
      const !v233 = PEQ(v158,DLC_Int 2);
      if v233 then {
        const !v234 = MUL(DLC_Int 2,v5);
        v232 = [v234,DLC_Int 0];
         }
      else {
        const !v235 = PEQ(v158,DLC_Int 0);
        if v235 then {
          const !v236 = MUL(DLC_Int 2,v5);
          v232 = [DLC_Int 0,v236];
           }
        else {
          v232 = [v5,v5];
           };
         };
      const !v237 = v232[0];
      const !v238 = v232[1];
      const !v239 = ADD(v6,v237);
      transfer.(v239).to(v7);
      transfer.(v238).to(v15);
      (halt! ) } },
  6 = {
    again(v15),
    (between [DLC_Int 10] []),
    last = 4,
    [v7, v5, v6, v15, v58, v95],
    [],
    {
      const !v135 = TXN_VALUE();
      const !v136 = PEQ(DLC_Int 0,v135);
      claim(CT_Require)(v136);
      const !v137 = BALANCE();
      transfer.(v137).to(v15);
      (halt! ) } },
  7 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 3,
    [v7, v5, v6, v15, v58],
    [],
    {
      const !v100 = TXN_VALUE();
      const !v101 = PEQ(DLC_Int 0,v100);
      claim(CT_Require)(v101);
      const !v102 = BALANCE();
      transfer.(v102).to(v7);
      (halt! ) } },
  8 = {
    again(v15),
    (between [DLC_Int 10] []),
    last = 2,
    [v7, v5, v6, v15],
    [],
    {
      const !v63 = TXN_VALUE();
      const !v64 = PEQ(DLC_Int 0,v63);
      claim(CT_Require)(v64);
      const !v65 = BALANCE();
      transfer.(v65).to(v15);
      (halt! ) } },
  9 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 1,
    [v7, v5, v6],
    [],
    {
      const !v20 = TXN_VALUE();
      const !v21 = PEQ(DLC_Int 0,v20);
      claim(CT_Require)(v21);
      const !v22 = BALANCE();
      transfer.(v22).to(v7);
      (halt! ) } }}