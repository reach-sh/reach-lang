#lang pl
{
  "A" = interact {
    commits = Fun([], Null),
    endsWith = Fun([Bytes], Null),
    getHand = Fun([], Bytes),
    getParams = Fun([], Tuple(UInt256, UInt256)),
    partnerIs = Fun([Address], Null),
    random = Fun([], UInt256),
    reveals = Fun([Bytes], Null)};
  const *v2 = interact("A")."getParams"();
  const *v3 = v2[0];
  const *v4 = v2[1];
  
  const !v8 = ADD(v3,v4);
  
  sendrecv join(v7) 1 (.publish((v3,v4), v8, ()))(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9,v10);
    claim(CT_Require)(v12);
    sendrecv join(v16) 2 ()()
      .timeout(DLC_Int 10, {
        
        sendrecv again(v7) 9 (.publish((), DLC_Int 0, (v7, v5, v6)))(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0,v22);
          claim(CT_Require)(v24);
          claim(CT_Require)(DLC_Bool True);
          eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      eff interact("A")."partnerIs"(v16);
      
      let v48;
      const *v49 = interact("A")."getHand"();
      const *v51 = BYTES_EQ(v49,DLC_Bytes "ROCK");
      const *v53 = BYTES_EQ(v49,DLC_Bytes "PAPER");
      const !v55 = BYTES_EQ(v49,DLC_Bytes "SCISSORS");
      const !v56 = IF_THEN_ELSE(v51,DLC_Bool True,v53);
      const !v57 = IF_THEN_ELSE(v56,DLC_Bool True,v55);
      claim(CT_Assume)(v57);
      if v51 then {
        v48 = DLC_Int 0;
         }
      else {
        if v53 then {
          v48 = DLC_Int 1;
           }
        else {
          v48 = DLC_Int 2;
           };
         };
      const *v64 = interact("A")."random"();
      const !v65 = digest(v64,v48);
      eff interact("A")."commits"();
      
      
      sendrecv again(v7) 3 (.publish((v65), DLC_Int 0, (v7, v5, v6, v16)))(v67)
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 8 ()(){
            const !v73 = TXN_VALUE();
            const !v75 = PEQ(DLC_Int 0,v73);
            claim(CT_Require)(v75);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v68 = TXN_VALUE();
        const !v70 = PEQ(DLC_Int 0,v68);
        claim(CT_Require)(v70);
        sendrecv again(v16) 4 ()(v112)
          .timeout(DLC_Int 10, {
            
            sendrecv again(v7) 7 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v67)))(){
              const !v118 = TXN_VALUE();
              const !v120 = PEQ(DLC_Int 0,v118);
              claim(CT_Require)(v120);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v113 = TXN_VALUE();
          const !v115 = PEQ(DLC_Int 0,v113);
          claim(CT_Require)(v115);
          const !v140 = PLE(DLC_Int 0,v112);
          const !v141 = PLT(v112,DLC_Int 3);
          const !v142 = IF_THEN_ELSE(v140,v141,DLC_Bool False);
          claim(CT_Require)(v142);
          let v144;
          const !v146 = PLE(DLC_Int 0,v112);
          const !v147 = PLT(v112,DLC_Int 3);
          const !v148 = IF_THEN_ELSE(v146,v147,DLC_Bool False);
          claim(CT_Require)(v148);
          const !v150 = PEQ(v112,DLC_Int 0);
          if v150 then {
            v144 = DLC_Bytes "ROCK";
             }
          else {
            const !v152 = PEQ(v112,DLC_Int 1);
            if v152 then {
              v144 = DLC_Bytes "PAPER";
               }
            else {
              v144 = DLC_Bytes "SCISSORS";
               };
             };
          eff interact("A")."reveals"(v144);
          
          
          sendrecv again(v7) 5 (.publish((v64,v48), DLC_Int 0, (v7, v5, v6, v16, v67, v112)))(v154, v155)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 6 ()(){
                const !v161 = TXN_VALUE();
                const !v163 = PEQ(DLC_Int 0,v161);
                claim(CT_Require)(v163);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v156 = TXN_VALUE();
            const !v158 = PEQ(DLC_Int 0,v156);
            claim(CT_Require)(v158);
            const !v183 = digest(v154,v155);
            const !v185 = PEQ(v67,v183);
            claim(CT_Require)(v185);
            const !v187 = PLE(DLC_Int 0,v155);
            const !v188 = PLT(v155,DLC_Int 3);
            const !v189 = IF_THEN_ELSE(v187,v188,DLC_Bool False);
            claim(CT_Require)(v189);
            let v191;
            const !v193 = PLE(DLC_Int 0,v155);
            const !v194 = PLT(v155,DLC_Int 3);
            const *v195 = IF_THEN_ELSE(v193,v194,DLC_Bool False);
            const !v197 = PLE(DLC_Int 0,v112);
            const !v198 = PLT(v112,DLC_Int 3);
            const *v199 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
            const !v200 = IF_THEN_ELSE(v195,v199,DLC_Bool False);
            if v200 then {
              const !v201 = SUB(DLC_Int 4,v112);
              const !v202 = ADD(v155,v201);
              const !v203 = MOD(v202,DLC_Int 3);
              v191 = v203;
               }
            else {
              if v195 then {
                v191 = DLC_Int 2;
                 }
              else {
                if v199 then {
                  v191 = DLC_Int 0;
                   }
                else {
                  v191 = DLC_Int 1;
                   };
                 };
               };
            let v261;
            const !v263 = PEQ(v191,DLC_Int 2);
            if v263 then {
              const !v264 = MUL(DLC_Int 2,v5);
              v261 = [v264,DLC_Int 0];
               }
            else {
              const !v266 = PEQ(v191,DLC_Int 0);
              if v266 then {
                const !v267 = MUL(DLC_Int 2,v5);
                v261 = [DLC_Int 0,v267];
                 }
              else {
                v261 = [v5,v5];
                 };
               };
            let v274;
            const !v276 = PLE(DLC_Int 0,v191);
            const !v277 = PLT(v191,DLC_Int 5);
            const !v278 = IF_THEN_ELSE(v276,v277,DLC_Bool False);
            claim(CT_Require)(v278);
            const !v280 = PEQ(v191,DLC_Int 0);
            if v280 then {
              v274 = DLC_Bytes "Bob wins";
               }
            else {
              const !v282 = PEQ(v191,DLC_Int 1);
              if v282 then {
                v274 = DLC_Bytes "Draw";
                 }
              else {
                const !v284 = PEQ(v191,DLC_Int 2);
                if v284 then {
                  v274 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v286 = PEQ(v191,DLC_Int 3);
                  if v286 then {
                    v274 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v274 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("A")."endsWith"(v274);
            
             } } } } },
  "B" = interact {
    acceptParams = Fun([UInt256, UInt256], Null),
    endsWith = Fun([Bytes], Null),
    getHand = Fun([], Bytes),
    partnerIs = Fun([Address], Null),
    random = Fun([], UInt256),
    shows = Fun([], Null)};
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9,v10);
    claim(CT_Require)(v12);
    eff interact("B")."partnerIs"(v7);
    eff interact("B")."acceptParams"(v5,v6);
    
    
    sendrecv join(v16) 2 (.publish((), v5, (v7, v5, v6)))()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 9 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0,v22);
          claim(CT_Require)(v24);
          claim(CT_Require)(DLC_Bool True);
          eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()(v67)
        .timeout(DLC_Int 10, {
          
          sendrecv again(v16) 8 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))(){
            const !v73 = TXN_VALUE();
            const !v75 = PEQ(DLC_Int 0,v73);
            claim(CT_Require)(v75);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v68 = TXN_VALUE();
        const !v70 = PEQ(DLC_Int 0,v68);
        claim(CT_Require)(v70);
        let v96;
        const *v97 = interact("B")."getHand"();
        const *v99 = BYTES_EQ(v97,DLC_Bytes "ROCK");
        const *v101 = BYTES_EQ(v97,DLC_Bytes "PAPER");
        const !v103 = BYTES_EQ(v97,DLC_Bytes "SCISSORS");
        const !v104 = IF_THEN_ELSE(v99,DLC_Bool True,v101);
        const !v105 = IF_THEN_ELSE(v104,DLC_Bool True,v103);
        claim(CT_Assume)(v105);
        if v99 then {
          v96 = DLC_Int 0;
           }
        else {
          if v101 then {
            v96 = DLC_Int 1;
             }
          else {
            v96 = DLC_Int 2;
             };
           };
        eff interact("B")."shows"();
        
        
        sendrecv again(v16) 4 (.publish((v96), DLC_Int 0, (v7, v5, v6, v16, v67)))(v112)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v118 = TXN_VALUE();
              const !v120 = PEQ(DLC_Int 0,v118);
              claim(CT_Require)(v120);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v113 = TXN_VALUE();
          const !v115 = PEQ(DLC_Int 0,v113);
          claim(CT_Require)(v115);
          const !v140 = PLE(DLC_Int 0,v112);
          const !v141 = PLT(v112,DLC_Int 3);
          const !v142 = IF_THEN_ELSE(v140,v141,DLC_Bool False);
          claim(CT_Require)(v142);
          sendrecv again(v7) 5 ()(v154, v155)
            .timeout(DLC_Int 10, {
              
              sendrecv again(v16) 6 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v67, v112)))(){
                const !v161 = TXN_VALUE();
                const !v163 = PEQ(DLC_Int 0,v161);
                claim(CT_Require)(v163);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v156 = TXN_VALUE();
            const !v158 = PEQ(DLC_Int 0,v156);
            claim(CT_Require)(v158);
            const !v183 = digest(v154,v155);
            const !v185 = PEQ(v67,v183);
            claim(CT_Require)(v185);
            const !v187 = PLE(DLC_Int 0,v155);
            const !v188 = PLT(v155,DLC_Int 3);
            const !v189 = IF_THEN_ELSE(v187,v188,DLC_Bool False);
            claim(CT_Require)(v189);
            let v191;
            const !v193 = PLE(DLC_Int 0,v155);
            const !v194 = PLT(v155,DLC_Int 3);
            const *v195 = IF_THEN_ELSE(v193,v194,DLC_Bool False);
            const !v197 = PLE(DLC_Int 0,v112);
            const !v198 = PLT(v112,DLC_Int 3);
            const *v199 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
            const !v200 = IF_THEN_ELSE(v195,v199,DLC_Bool False);
            if v200 then {
              const !v201 = SUB(DLC_Int 4,v112);
              const !v202 = ADD(v155,v201);
              const !v203 = MOD(v202,DLC_Int 3);
              v191 = v203;
               }
            else {
              if v195 then {
                v191 = DLC_Int 2;
                 }
              else {
                if v199 then {
                  v191 = DLC_Int 0;
                   }
                else {
                  v191 = DLC_Int 1;
                   };
                 };
               };
            let v261;
            const !v263 = PEQ(v191,DLC_Int 2);
            if v263 then {
              const !v264 = MUL(DLC_Int 2,v5);
              v261 = [v264,DLC_Int 0];
               }
            else {
              const !v266 = PEQ(v191,DLC_Int 0);
              if v266 then {
                const !v267 = MUL(DLC_Int 2,v5);
                v261 = [DLC_Int 0,v267];
                 }
              else {
                v261 = [v5,v5];
                 };
               };
            let v289;
            const !v291 = PLE(DLC_Int 0,v191);
            const !v292 = PLT(v191,DLC_Int 5);
            const !v293 = IF_THEN_ELSE(v291,v292,DLC_Bool False);
            claim(CT_Require)(v293);
            const !v295 = PEQ(v191,DLC_Int 0);
            if v295 then {
              v289 = DLC_Bytes "Bob wins";
               }
            else {
              const !v297 = PEQ(v191,DLC_Int 1);
              if v297 then {
                v289 = DLC_Bytes "Draw";
                 }
              else {
                const !v299 = PEQ(v191,DLC_Int 2);
                if v299 then {
                  v289 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v301 = PEQ(v191,DLC_Int 3);
                  if v301 then {
                    v289 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v289 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("B")."endsWith"(v289);
            
             } } } } },
  "O" = interact {
    };
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9,v10);
    claim(CT_Require)(v12);
    sendrecv join(v16) 2 ()()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 9 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0,v22);
          claim(CT_Require)(v24);
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()(v67)
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 8 ()(){
            const !v73 = TXN_VALUE();
            const !v75 = PEQ(DLC_Int 0,v73);
            claim(CT_Require)(v75);
             } }){
        const !v68 = TXN_VALUE();
        const !v70 = PEQ(DLC_Int 0,v68);
        claim(CT_Require)(v70);
        sendrecv again(v16) 4 ()(v112)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v118 = TXN_VALUE();
              const !v120 = PEQ(DLC_Int 0,v118);
              claim(CT_Require)(v120);
               } }){
          const !v113 = TXN_VALUE();
          const !v115 = PEQ(DLC_Int 0,v113);
          claim(CT_Require)(v115);
          const !v140 = PLE(DLC_Int 0,v112);
          const !v141 = PLT(v112,DLC_Int 3);
          const !v142 = IF_THEN_ELSE(v140,v141,DLC_Bool False);
          claim(CT_Require)(v142);
          sendrecv again(v7) 5 ()(v154, v155)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 6 ()(){
                const !v161 = TXN_VALUE();
                const !v163 = PEQ(DLC_Int 0,v161);
                claim(CT_Require)(v163);
                 } }){
            const !v156 = TXN_VALUE();
            const !v158 = PEQ(DLC_Int 0,v156);
            claim(CT_Require)(v158);
            const !v183 = digest(v154,v155);
            const !v185 = PEQ(v67,v183);
            claim(CT_Require)(v185);
            const !v187 = PLE(DLC_Int 0,v155);
            const !v188 = PLT(v155,DLC_Int 3);
            const !v189 = IF_THEN_ELSE(v187,v188,DLC_Bool False);
            claim(CT_Require)(v189);
            let v191;
            const !v193 = PLE(DLC_Int 0,v155);
            const !v194 = PLT(v155,DLC_Int 3);
            const *v195 = IF_THEN_ELSE(v193,v194,DLC_Bool False);
            const !v197 = PLE(DLC_Int 0,v112);
            const !v198 = PLT(v112,DLC_Int 3);
            const *v199 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
            const !v200 = IF_THEN_ELSE(v195,v199,DLC_Bool False);
            if v200 then {
              const !v201 = SUB(DLC_Int 4,v112);
              const !v202 = ADD(v155,v201);
              const !v203 = MOD(v202,DLC_Int 3);
              v191 = v203;
               }
            else {
              if v195 then {
                v191 = DLC_Int 2;
                 }
              else {
                if v199 then {
                  v191 = DLC_Int 0;
                   }
                else {
                  v191 = DLC_Int 1;
                   };
                 };
               };
            let v261;
            const !v263 = PEQ(v191,DLC_Int 2);
            if v263 then {
              const !v264 = MUL(DLC_Int 2,v5);
              v261 = [v264,DLC_Int 0];
               }
            else {
              const !v266 = PEQ(v191,DLC_Int 0);
              if v266 then {
                const !v267 = MUL(DLC_Int 2,v5);
                v261 = [DLC_Int 0,v267];
                 }
              else {
                v261 = [v5,v5];
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
      const !v12 = PEQ(v9,v10);
      claim(CT_Require)(v12);
      (wait! [v7, v5, v6]) } },
  2 = {
    join(v16),
    (between [] [DLC_Int 10]),
    last = 1,
    [v7, v5, v6],
    [],
    {
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      (wait! [v7, v5, v6, v16]) } },
  3 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 2,
    [v7, v5, v6, v16],
    [v67],
    {
      const !v68 = TXN_VALUE();
      const !v70 = PEQ(DLC_Int 0,v68);
      claim(CT_Require)(v70);
      (wait! [v7, v5, v6, v16, v67]) } },
  4 = {
    again(v16),
    (between [] [DLC_Int 10]),
    last = 3,
    [v7, v5, v6, v16, v67],
    [v112],
    {
      const !v113 = TXN_VALUE();
      const !v115 = PEQ(DLC_Int 0,v113);
      claim(CT_Require)(v115);
      const !v140 = PLE(DLC_Int 0,v112);
      const !v141 = PLT(v112,DLC_Int 3);
      const !v142 = IF_THEN_ELSE(v140,v141,DLC_Bool False);
      claim(CT_Require)(v142);
      (wait! [v7, v5, v6, v16, v67, v112]) } },
  5 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 4,
    [v7, v5, v6, v16, v67, v112],
    [v154, v155],
    {
      const !v156 = TXN_VALUE();
      const !v158 = PEQ(DLC_Int 0,v156);
      claim(CT_Require)(v158);
      const !v183 = digest(v154,v155);
      const !v185 = PEQ(v67,v183);
      claim(CT_Require)(v185);
      const !v187 = PLE(DLC_Int 0,v155);
      const !v188 = PLT(v155,DLC_Int 3);
      const !v189 = IF_THEN_ELSE(v187,v188,DLC_Bool False);
      claim(CT_Require)(v189);
      let v191;
      const !v193 = PLE(DLC_Int 0,v155);
      const !v194 = PLT(v155,DLC_Int 3);
      const *v195 = IF_THEN_ELSE(v193,v194,DLC_Bool False);
      const !v197 = PLE(DLC_Int 0,v112);
      const !v198 = PLT(v112,DLC_Int 3);
      const *v199 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
      const !v200 = IF_THEN_ELSE(v195,v199,DLC_Bool False);
      if v200 then {
        const !v201 = SUB(DLC_Int 4,v112);
        const !v202 = ADD(v155,v201);
        const !v203 = MOD(v202,DLC_Int 3);
        v191 = v203;
         }
      else {
        if v195 then {
          v191 = DLC_Int 2;
           }
        else {
          if v199 then {
            v191 = DLC_Int 0;
             }
          else {
            v191 = DLC_Int 1;
             };
           };
         };
      let v261;
      const !v263 = PEQ(v191,DLC_Int 2);
      if v263 then {
        const !v264 = MUL(DLC_Int 2,v5);
        v261 = [v264,DLC_Int 0];
         }
      else {
        const !v266 = PEQ(v191,DLC_Int 0);
        if v266 then {
          const !v267 = MUL(DLC_Int 2,v5);
          v261 = [DLC_Int 0,v267];
           }
        else {
          v261 = [v5,v5];
           };
         };
      const !v268 = v261[0];
      const !v269 = v261[1];
      const !v270 = ADD(v6,v268);
      transfer.(v270).to(v7);
      transfer.(v269).to(v16);
      (halt! ) } },
  6 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 4,
    [v7, v5, v6, v16, v67, v112],
    [],
    {
      const !v161 = TXN_VALUE();
      const !v163 = PEQ(DLC_Int 0,v161);
      claim(CT_Require)(v163);
      const !v164 = BALANCE();
      transfer.(v164).to(v16);
      (halt! ) } },
  7 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 3,
    [v7, v5, v6, v16, v67],
    [],
    {
      const !v118 = TXN_VALUE();
      const !v120 = PEQ(DLC_Int 0,v118);
      claim(CT_Require)(v120);
      const !v121 = BALANCE();
      transfer.(v121).to(v7);
      (halt! ) } },
  8 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 2,
    [v7, v5, v6, v16],
    [],
    {
      const !v73 = TXN_VALUE();
      const !v75 = PEQ(DLC_Int 0,v73);
      claim(CT_Require)(v75);
      const !v76 = BALANCE();
      transfer.(v76).to(v16);
      (halt! ) } },
  9 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 1,
    [v7, v5, v6],
    [],
    {
      const !v22 = TXN_VALUE();
      const !v24 = PEQ(DLC_Int 0,v22);
      claim(CT_Require)(v24);
      const !v25 = BALANCE();
      transfer.(v25).to(v7);
      (halt! ) } }}