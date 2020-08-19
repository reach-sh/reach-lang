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
      
      let v50;
      const *v51 = interact("A")."getHand"();
      const *v53 = BYTES_EQ(v51,DLC_Bytes "ROCK");
      const *v55 = BYTES_EQ(v51,DLC_Bytes "PAPER");
      const !v57 = BYTES_EQ(v51,DLC_Bytes "SCISSORS");
      const !v59 = IF_THEN_ELSE(v53,DLC_Bool True,v55);
      const !v61 = IF_THEN_ELSE(v59,DLC_Bool True,v57);
      claim(CT_Assume)(v61);
      if v53 then {
        v50 = DLC_Int 0;
         }
      else {
        if v55 then {
          v50 = DLC_Int 1;
           }
        else {
          v50 = DLC_Int 2;
           };
         };
      const *v69 = interact("A")."random"();
      const !v70 = digest(v69,v50);
      eff interact("A")."commits"();
      
      
      sendrecv again(v7) 3 (.publish((v70), DLC_Int 0, (v7, v5, v6, v16)))(v72)
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 8 ()(){
            const !v78 = TXN_VALUE();
            const !v80 = PEQ(DLC_Int 0,v78);
            claim(CT_Require)(v80);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v73 = TXN_VALUE();
        const !v75 = PEQ(DLC_Int 0,v73);
        claim(CT_Require)(v75);
        sendrecv again(v16) 4 ()(v122)
          .timeout(DLC_Int 10, {
            
            sendrecv again(v7) 7 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v72)))(){
              const !v128 = TXN_VALUE();
              const !v130 = PEQ(DLC_Int 0,v128);
              claim(CT_Require)(v130);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v123 = TXN_VALUE();
          const !v125 = PEQ(DLC_Int 0,v123);
          claim(CT_Require)(v125);
          const !v152 = PLE(DLC_Int 0,v122);
          const !v153 = PLT(v122,DLC_Int 3);
          const !v155 = IF_THEN_ELSE(v152,v153,DLC_Bool False);
          claim(CT_Require)(v155);
          let v157;
          const !v159 = PLE(DLC_Int 0,v122);
          const !v160 = PLT(v122,DLC_Int 3);
          const !v162 = IF_THEN_ELSE(v159,v160,DLC_Bool False);
          claim(CT_Require)(v162);
          const !v164 = PEQ(v122,DLC_Int 0);
          if v164 then {
            v157 = DLC_Bytes "ROCK";
             }
          else {
            const !v166 = PEQ(v122,DLC_Int 1);
            if v166 then {
              v157 = DLC_Bytes "PAPER";
               }
            else {
              v157 = DLC_Bytes "SCISSORS";
               };
             };
          eff interact("A")."reveals"(v157);
          
          
          sendrecv again(v7) 5 (.publish((v69,v50), DLC_Int 0, (v7, v5, v6, v16, v72, v122)))(v168, v169)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 6 ()(){
                const !v175 = TXN_VALUE();
                const !v177 = PEQ(DLC_Int 0,v175);
                claim(CT_Require)(v177);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v170 = TXN_VALUE();
            const !v172 = PEQ(DLC_Int 0,v170);
            claim(CT_Require)(v172);
            const !v199 = digest(v168,v169);
            const !v201 = PEQ(v72,v199);
            claim(CT_Require)(v201);
            const !v203 = PLE(DLC_Int 0,v169);
            const !v204 = PLT(v169,DLC_Int 3);
            const !v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
            claim(CT_Require)(v206);
            let v208;
            const !v210 = PLE(DLC_Int 0,v169);
            const !v211 = PLT(v169,DLC_Int 3);
            const *v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
            const !v215 = PLE(DLC_Int 0,v122);
            const !v216 = PLT(v122,DLC_Int 3);
            const *v218 = IF_THEN_ELSE(v215,v216,DLC_Bool False);
            const !v220 = IF_THEN_ELSE(v213,v218,DLC_Bool False);
            if v220 then {
              const !v221 = SUB(DLC_Int 4,v122);
              const !v222 = ADD(v169,v221);
              const !v223 = MOD(v222,DLC_Int 3);
              v208 = v223;
               }
            else {
              if v213 then {
                v208 = DLC_Int 2;
                 }
              else {
                if v218 then {
                  v208 = DLC_Int 0;
                   }
                else {
                  v208 = DLC_Int 1;
                   };
                 };
               };
            let v292;
            const !v294 = PEQ(v208,DLC_Int 2);
            if v294 then {
              const !v295 = MUL(DLC_Int 2,v5);
              v292 = [v295,DLC_Int 0];
               }
            else {
              const !v297 = PEQ(v208,DLC_Int 0);
              if v297 then {
                const !v298 = MUL(DLC_Int 2,v5);
                v292 = [DLC_Int 0,v298];
                 }
              else {
                v292 = [v5,v5];
                 };
               };
            let v305;
            const !v307 = PLE(DLC_Int 0,v208);
            const !v308 = PLT(v208,DLC_Int 5);
            const !v310 = IF_THEN_ELSE(v307,v308,DLC_Bool False);
            claim(CT_Require)(v310);
            const !v312 = PEQ(v208,DLC_Int 0);
            if v312 then {
              v305 = DLC_Bytes "Bob wins";
               }
            else {
              const !v314 = PEQ(v208,DLC_Int 1);
              if v314 then {
                v305 = DLC_Bytes "Draw";
                 }
              else {
                const !v316 = PEQ(v208,DLC_Int 2);
                if v316 then {
                  v305 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v318 = PEQ(v208,DLC_Int 3);
                  if v318 then {
                    v305 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v305 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("A")."endsWith"(v305);
            
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
      sendrecv again(v7) 3 ()(v72)
        .timeout(DLC_Int 10, {
          
          sendrecv again(v16) 8 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))(){
            const !v78 = TXN_VALUE();
            const !v80 = PEQ(DLC_Int 0,v78);
            claim(CT_Require)(v80);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v73 = TXN_VALUE();
        const !v75 = PEQ(DLC_Int 0,v73);
        claim(CT_Require)(v75);
        let v103;
        const *v104 = interact("B")."getHand"();
        const *v106 = BYTES_EQ(v104,DLC_Bytes "ROCK");
        const *v108 = BYTES_EQ(v104,DLC_Bytes "PAPER");
        const !v110 = BYTES_EQ(v104,DLC_Bytes "SCISSORS");
        const !v112 = IF_THEN_ELSE(v106,DLC_Bool True,v108);
        const !v114 = IF_THEN_ELSE(v112,DLC_Bool True,v110);
        claim(CT_Assume)(v114);
        if v106 then {
          v103 = DLC_Int 0;
           }
        else {
          if v108 then {
            v103 = DLC_Int 1;
             }
          else {
            v103 = DLC_Int 2;
             };
           };
        eff interact("B")."shows"();
        
        
        sendrecv again(v16) 4 (.publish((v103), DLC_Int 0, (v7, v5, v6, v16, v72)))(v122)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v128 = TXN_VALUE();
              const !v130 = PEQ(DLC_Int 0,v128);
              claim(CT_Require)(v130);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
          const !v123 = TXN_VALUE();
          const !v125 = PEQ(DLC_Int 0,v123);
          claim(CT_Require)(v125);
          const !v152 = PLE(DLC_Int 0,v122);
          const !v153 = PLT(v122,DLC_Int 3);
          const !v155 = IF_THEN_ELSE(v152,v153,DLC_Bool False);
          claim(CT_Require)(v155);
          sendrecv again(v7) 5 ()(v168, v169)
            .timeout(DLC_Int 10, {
              
              sendrecv again(v16) 6 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v72, v122)))(){
                const !v175 = TXN_VALUE();
                const !v177 = PEQ(DLC_Int 0,v175);
                claim(CT_Require)(v177);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v170 = TXN_VALUE();
            const !v172 = PEQ(DLC_Int 0,v170);
            claim(CT_Require)(v172);
            const !v199 = digest(v168,v169);
            const !v201 = PEQ(v72,v199);
            claim(CT_Require)(v201);
            const !v203 = PLE(DLC_Int 0,v169);
            const !v204 = PLT(v169,DLC_Int 3);
            const !v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
            claim(CT_Require)(v206);
            let v208;
            const !v210 = PLE(DLC_Int 0,v169);
            const !v211 = PLT(v169,DLC_Int 3);
            const *v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
            const !v215 = PLE(DLC_Int 0,v122);
            const !v216 = PLT(v122,DLC_Int 3);
            const *v218 = IF_THEN_ELSE(v215,v216,DLC_Bool False);
            const !v220 = IF_THEN_ELSE(v213,v218,DLC_Bool False);
            if v220 then {
              const !v221 = SUB(DLC_Int 4,v122);
              const !v222 = ADD(v169,v221);
              const !v223 = MOD(v222,DLC_Int 3);
              v208 = v223;
               }
            else {
              if v213 then {
                v208 = DLC_Int 2;
                 }
              else {
                if v218 then {
                  v208 = DLC_Int 0;
                   }
                else {
                  v208 = DLC_Int 1;
                   };
                 };
               };
            let v292;
            const !v294 = PEQ(v208,DLC_Int 2);
            if v294 then {
              const !v295 = MUL(DLC_Int 2,v5);
              v292 = [v295,DLC_Int 0];
               }
            else {
              const !v297 = PEQ(v208,DLC_Int 0);
              if v297 then {
                const !v298 = MUL(DLC_Int 2,v5);
                v292 = [DLC_Int 0,v298];
                 }
              else {
                v292 = [v5,v5];
                 };
               };
            let v321;
            const !v323 = PLE(DLC_Int 0,v208);
            const !v324 = PLT(v208,DLC_Int 5);
            const !v326 = IF_THEN_ELSE(v323,v324,DLC_Bool False);
            claim(CT_Require)(v326);
            const !v328 = PEQ(v208,DLC_Int 0);
            if v328 then {
              v321 = DLC_Bytes "Bob wins";
               }
            else {
              const !v330 = PEQ(v208,DLC_Int 1);
              if v330 then {
                v321 = DLC_Bytes "Draw";
                 }
              else {
                const !v332 = PEQ(v208,DLC_Int 2);
                if v332 then {
                  v321 = DLC_Bytes "Alice wins";
                   }
                else {
                  const !v334 = PEQ(v208,DLC_Int 3);
                  if v334 then {
                    v321 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v321 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            eff interact("B")."endsWith"(v321);
            
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
      sendrecv again(v7) 3 ()(v72)
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 8 ()(){
            const !v78 = TXN_VALUE();
            const !v80 = PEQ(DLC_Int 0,v78);
            claim(CT_Require)(v80);
             } }){
        const !v73 = TXN_VALUE();
        const !v75 = PEQ(DLC_Int 0,v73);
        claim(CT_Require)(v75);
        sendrecv again(v16) 4 ()(v122)
          .timeout(DLC_Int 10, {
            sendrecv again(v7) 7 ()(){
              const !v128 = TXN_VALUE();
              const !v130 = PEQ(DLC_Int 0,v128);
              claim(CT_Require)(v130);
               } }){
          const !v123 = TXN_VALUE();
          const !v125 = PEQ(DLC_Int 0,v123);
          claim(CT_Require)(v125);
          const !v152 = PLE(DLC_Int 0,v122);
          const !v153 = PLT(v122,DLC_Int 3);
          const !v155 = IF_THEN_ELSE(v152,v153,DLC_Bool False);
          claim(CT_Require)(v155);
          sendrecv again(v7) 5 ()(v168, v169)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 6 ()(){
                const !v175 = TXN_VALUE();
                const !v177 = PEQ(DLC_Int 0,v175);
                claim(CT_Require)(v177);
                 } }){
            const !v170 = TXN_VALUE();
            const !v172 = PEQ(DLC_Int 0,v170);
            claim(CT_Require)(v172);
            const !v199 = digest(v168,v169);
            const !v201 = PEQ(v72,v199);
            claim(CT_Require)(v201);
            const !v203 = PLE(DLC_Int 0,v169);
            const !v204 = PLT(v169,DLC_Int 3);
            const !v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
            claim(CT_Require)(v206);
            let v208;
            const !v210 = PLE(DLC_Int 0,v169);
            const !v211 = PLT(v169,DLC_Int 3);
            const *v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
            const !v215 = PLE(DLC_Int 0,v122);
            const !v216 = PLT(v122,DLC_Int 3);
            const *v218 = IF_THEN_ELSE(v215,v216,DLC_Bool False);
            const !v220 = IF_THEN_ELSE(v213,v218,DLC_Bool False);
            if v220 then {
              const !v221 = SUB(DLC_Int 4,v122);
              const !v222 = ADD(v169,v221);
              const !v223 = MOD(v222,DLC_Int 3);
              v208 = v223;
               }
            else {
              if v213 then {
                v208 = DLC_Int 2;
                 }
              else {
                if v218 then {
                  v208 = DLC_Int 0;
                   }
                else {
                  v208 = DLC_Int 1;
                   };
                 };
               };
            let v292;
            const !v294 = PEQ(v208,DLC_Int 2);
            if v294 then {
              const !v295 = MUL(DLC_Int 2,v5);
              v292 = [v295,DLC_Int 0];
               }
            else {
              const !v297 = PEQ(v208,DLC_Int 0);
              if v297 then {
                const !v298 = MUL(DLC_Int 2,v5);
                v292 = [DLC_Int 0,v298];
                 }
              else {
                v292 = [v5,v5];
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
    [v72],
    {
      const !v73 = TXN_VALUE();
      const !v75 = PEQ(DLC_Int 0,v73);
      claim(CT_Require)(v75);
      (wait! [v7, v5, v6, v16, v72]) } },
  4 = {
    again(v16),
    (between [] [DLC_Int 10]),
    last = 3,
    [v7, v5, v6, v16, v72],
    [v122],
    {
      const !v123 = TXN_VALUE();
      const !v125 = PEQ(DLC_Int 0,v123);
      claim(CT_Require)(v125);
      const !v152 = PLE(DLC_Int 0,v122);
      const !v153 = PLT(v122,DLC_Int 3);
      const !v155 = IF_THEN_ELSE(v152,v153,DLC_Bool False);
      claim(CT_Require)(v155);
      (wait! [v7, v5, v6, v16, v72, v122]) } },
  5 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 4,
    [v7, v5, v6, v16, v72, v122],
    [v168, v169],
    {
      const !v170 = TXN_VALUE();
      const !v172 = PEQ(DLC_Int 0,v170);
      claim(CT_Require)(v172);
      const !v199 = digest(v168,v169);
      const !v201 = PEQ(v72,v199);
      claim(CT_Require)(v201);
      const !v203 = PLE(DLC_Int 0,v169);
      const !v204 = PLT(v169,DLC_Int 3);
      const !v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
      claim(CT_Require)(v206);
      let v208;
      const !v210 = PLE(DLC_Int 0,v169);
      const !v211 = PLT(v169,DLC_Int 3);
      const *v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
      const !v215 = PLE(DLC_Int 0,v122);
      const !v216 = PLT(v122,DLC_Int 3);
      const *v218 = IF_THEN_ELSE(v215,v216,DLC_Bool False);
      const !v220 = IF_THEN_ELSE(v213,v218,DLC_Bool False);
      if v220 then {
        const !v221 = SUB(DLC_Int 4,v122);
        const !v222 = ADD(v169,v221);
        const !v223 = MOD(v222,DLC_Int 3);
        v208 = v223;
         }
      else {
        if v213 then {
          v208 = DLC_Int 2;
           }
        else {
          if v218 then {
            v208 = DLC_Int 0;
             }
          else {
            v208 = DLC_Int 1;
             };
           };
         };
      let v292;
      const !v294 = PEQ(v208,DLC_Int 2);
      if v294 then {
        const !v295 = MUL(DLC_Int 2,v5);
        v292 = [v295,DLC_Int 0];
         }
      else {
        const !v297 = PEQ(v208,DLC_Int 0);
        if v297 then {
          const !v298 = MUL(DLC_Int 2,v5);
          v292 = [DLC_Int 0,v298];
           }
        else {
          v292 = [v5,v5];
           };
         };
      const !v299 = v292[0];
      const !v300 = v292[1];
      const !v301 = ADD(v6,v299);
      transfer.(v301).to(v7);
      transfer.(v300).to(v16);
      (halt! ) } },
  6 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 4,
    [v7, v5, v6, v16, v72, v122],
    [],
    {
      const !v175 = TXN_VALUE();
      const !v177 = PEQ(DLC_Int 0,v175);
      claim(CT_Require)(v177);
      const !v178 = BALANCE();
      transfer.(v178).to(v16);
      (halt! ) } },
  7 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 3,
    [v7, v5, v6, v16, v72],
    [],
    {
      const !v128 = TXN_VALUE();
      const !v130 = PEQ(DLC_Int 0,v128);
      claim(CT_Require)(v130);
      const !v131 = BALANCE();
      transfer.(v131).to(v7);
      (halt! ) } },
  8 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 2,
    [v7, v5, v6, v16],
    [],
    {
      const !v78 = TXN_VALUE();
      const !v80 = PEQ(DLC_Int 0,v78);
      claim(CT_Require)(v80);
      const !v81 = BALANCE();
      transfer.(v81).to(v16);
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