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
        
        sendrecv again(v7) 12 (.publish((), DLC_Int 0, (v7, v5, v6)))(){
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
      
      
      sendrecv again(v7) 3 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))()
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 11 ()(){
            const !v52 = TXN_VALUE();
            const !v54 = PEQ(DLC_Int 0,v52);
            claim(CT_Require)(v54);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v47 = TXN_VALUE();
        const !v49 = PEQ(DLC_Int 0,v47);
        claim(CT_Require)(v49);
        loopvar {
          v75 = DLC_Int 0,
          v76 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v104 = PEQ(v76,DLC_Int 1);
           v104) }
        {
          let v108;
          const *v109 = interact("A")."getHand"();
          const *v111 = BYTES_EQ(v109,DLC_Bytes "ROCK");
          const *v113 = BYTES_EQ(v109,DLC_Bytes "PAPER");
          const !v115 = BYTES_EQ(v109,DLC_Bytes "SCISSORS");
          const !v117 = IF_THEN_ELSE(v111,DLC_Bool True,v113);
          const !v119 = IF_THEN_ELSE(v117,DLC_Bool True,v115);
          claim(CT_Assume)(v119);
          if v111 then {
            v108 = DLC_Int 0;
             }
          else {
            if v113 then {
              v108 = DLC_Int 1;
               }
            else {
              v108 = DLC_Int 2;
               };
             };
          const *v127 = interact("A")."random"();
          const !v128 = digest(v127,v108);
          eff interact("A")."commits"();
          
          
          sendrecv again(v7) 5 (.publish((v128), DLC_Int 0, (v7, v5, v6, v16, v75)))(v130)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 10 ()(){
                const !v136 = TXN_VALUE();
                const !v138 = PEQ(DLC_Int 0,v136);
                claim(CT_Require)(v138);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v131 = TXN_VALUE();
            const !v133 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v133);
            sendrecv again(v16) 6 ()(v180)
              .timeout(DLC_Int 10, {
                
                sendrecv again(v7) 9 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v130, v75)))(){
                  const !v186 = TXN_VALUE();
                  const !v188 = PEQ(DLC_Int 0,v186);
                  claim(CT_Require)(v188);
                  claim(CT_Require)(DLC_Bool True);
                  eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
                  
                   } }){
              const !v181 = TXN_VALUE();
              const !v183 = PEQ(DLC_Int 0,v181);
              claim(CT_Require)(v183);
              const !v210 = PLE(DLC_Int 0,v180);
              const !v211 = PLT(v180,DLC_Int 3);
              const !v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
              claim(CT_Require)(v213);
              let v215;
              const !v217 = PLE(DLC_Int 0,v180);
              const !v218 = PLT(v180,DLC_Int 3);
              const !v220 = IF_THEN_ELSE(v217,v218,DLC_Bool False);
              claim(CT_Require)(v220);
              const !v222 = PEQ(v180,DLC_Int 0);
              if v222 then {
                v215 = DLC_Bytes "ROCK";
                 }
              else {
                const !v224 = PEQ(v180,DLC_Int 1);
                if v224 then {
                  v215 = DLC_Bytes "PAPER";
                   }
                else {
                  v215 = DLC_Bytes "SCISSORS";
                   };
                 };
              eff interact("A")."reveals"(v215);
              
              
              sendrecv again(v7) 7 (.publish((v127,v108), DLC_Int 0, (v7, v5, v6, v16, v130, v180, v75)))(v226, v227)
                .timeout(DLC_Int 10, {
                  sendrecv again(v16) 8 ()(){
                    const !v233 = TXN_VALUE();
                    const !v235 = PEQ(DLC_Int 0,v233);
                    claim(CT_Require)(v235);
                    claim(CT_Require)(DLC_Bool True);
                    eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                    
                     } }){
                const !v228 = TXN_VALUE();
                const !v230 = PEQ(DLC_Int 0,v228);
                claim(CT_Require)(v230);
                const !v257 = digest(v226,v227);
                const !v259 = PEQ(v130,v257);
                claim(CT_Require)(v259);
                const !v261 = PLE(DLC_Int 0,v227);
                const !v262 = PLT(v227,DLC_Int 3);
                const !v264 = IF_THEN_ELSE(v261,v262,DLC_Bool False);
                claim(CT_Require)(v264);
                let v266;
                const !v268 = PLE(DLC_Int 0,v227);
                const !v269 = PLT(v227,DLC_Int 3);
                const *v271 = IF_THEN_ELSE(v268,v269,DLC_Bool False);
                const !v273 = PLE(DLC_Int 0,v180);
                const !v274 = PLT(v180,DLC_Int 3);
                const *v276 = IF_THEN_ELSE(v273,v274,DLC_Bool False);
                const !v278 = IF_THEN_ELSE(v271,v276,DLC_Bool False);
                if v278 then {
                  const !v279 = SUB(DLC_Int 4,v180);
                  const !v280 = ADD(v227,v279);
                  const !v281 = MOD(v280,DLC_Int 3);
                  v266 = v281;
                   }
                else {
                  if v271 then {
                    v266 = DLC_Int 2;
                     }
                  else {
                    if v276 then {
                      v266 = DLC_Int 0;
                       }
                    else {
                      v266 = DLC_Int 1;
                       };
                     };
                   };
                const !v349 = ADD(DLC_Int 1,v75);
                {
                  v75 = v349,
                  v76 = v266}
                continue; } } } }
        let v356;
        const !v358 = PEQ(v76,DLC_Int 2);
        if v358 then {
          const !v359 = MUL(DLC_Int 2,v5);
          v356 = [v359,DLC_Int 0];
           }
        else {
          const !v361 = PEQ(v76,DLC_Int 0);
          if v361 then {
            const !v362 = MUL(DLC_Int 2,v5);
            v356 = [DLC_Int 0,v362];
             }
          else {
            v356 = [v5,v5];
             };
           };
        let v369;
        const !v371 = PLE(DLC_Int 0,v76);
        const !v372 = PLT(v76,DLC_Int 5);
        const !v374 = IF_THEN_ELSE(v371,v372,DLC_Bool False);
        claim(CT_Require)(v374);
        const !v376 = PEQ(v76,DLC_Int 0);
        if v376 then {
          v369 = DLC_Bytes "Bob wins";
           }
        else {
          const !v378 = PEQ(v76,DLC_Int 1);
          if v378 then {
            v369 = DLC_Bytes "Draw";
             }
          else {
            const !v380 = PEQ(v76,DLC_Int 2);
            if v380 then {
              v369 = DLC_Bytes "Alice wins";
               }
            else {
              const !v382 = PEQ(v76,DLC_Int 3);
              if v382 then {
                v369 = DLC_Bytes "Alice quits";
                 }
              else {
                v369 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        eff interact("A")."endsWith"(v369);
        
         } } },
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
        sendrecv again(v7) 12 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0,v22);
          claim(CT_Require)(v24);
          claim(CT_Require)(DLC_Bool True);
          eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()()
        .timeout(DLC_Int 10, {
          
          sendrecv again(v16) 11 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))(){
            const !v52 = TXN_VALUE();
            const !v54 = PEQ(DLC_Int 0,v52);
            claim(CT_Require)(v54);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v47 = TXN_VALUE();
        const !v49 = PEQ(DLC_Int 0,v47);
        claim(CT_Require)(v49);
        loopvar {
          v75 = DLC_Int 0,
          v76 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v104 = PEQ(v76,DLC_Int 1);
           v104) }
        {
          sendrecv again(v7) 5 ()(v130)
            .timeout(DLC_Int 10, {
              
              sendrecv again(v16) 10 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v75)))(){
                const !v136 = TXN_VALUE();
                const !v138 = PEQ(DLC_Int 0,v136);
                claim(CT_Require)(v138);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v131 = TXN_VALUE();
            const !v133 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v133);
            let v161;
            const *v162 = interact("B")."getHand"();
            const *v164 = BYTES_EQ(v162,DLC_Bytes "ROCK");
            const *v166 = BYTES_EQ(v162,DLC_Bytes "PAPER");
            const !v168 = BYTES_EQ(v162,DLC_Bytes "SCISSORS");
            const !v170 = IF_THEN_ELSE(v164,DLC_Bool True,v166);
            const !v172 = IF_THEN_ELSE(v170,DLC_Bool True,v168);
            claim(CT_Assume)(v172);
            if v164 then {
              v161 = DLC_Int 0;
               }
            else {
              if v166 then {
                v161 = DLC_Int 1;
                 }
              else {
                v161 = DLC_Int 2;
                 };
               };
            eff interact("B")."shows"();
            
            
            sendrecv again(v16) 6 (.publish((v161), DLC_Int 0, (v7, v5, v6, v16, v130, v75)))(v180)
              .timeout(DLC_Int 10, {
                sendrecv again(v7) 9 ()(){
                  const !v186 = TXN_VALUE();
                  const !v188 = PEQ(DLC_Int 0,v186);
                  claim(CT_Require)(v188);
                  claim(CT_Require)(DLC_Bool True);
                  eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
                  
                   } }){
              const !v181 = TXN_VALUE();
              const !v183 = PEQ(DLC_Int 0,v181);
              claim(CT_Require)(v183);
              const !v210 = PLE(DLC_Int 0,v180);
              const !v211 = PLT(v180,DLC_Int 3);
              const !v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
              claim(CT_Require)(v213);
              sendrecv again(v7) 7 ()(v226, v227)
                .timeout(DLC_Int 10, {
                  
                  sendrecv again(v16) 8 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v130, v180, v75)))(){
                    const !v233 = TXN_VALUE();
                    const !v235 = PEQ(DLC_Int 0,v233);
                    claim(CT_Require)(v235);
                    claim(CT_Require)(DLC_Bool True);
                    eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                    
                     } }){
                const !v228 = TXN_VALUE();
                const !v230 = PEQ(DLC_Int 0,v228);
                claim(CT_Require)(v230);
                const !v257 = digest(v226,v227);
                const !v259 = PEQ(v130,v257);
                claim(CT_Require)(v259);
                const !v261 = PLE(DLC_Int 0,v227);
                const !v262 = PLT(v227,DLC_Int 3);
                const !v264 = IF_THEN_ELSE(v261,v262,DLC_Bool False);
                claim(CT_Require)(v264);
                let v266;
                const !v268 = PLE(DLC_Int 0,v227);
                const !v269 = PLT(v227,DLC_Int 3);
                const *v271 = IF_THEN_ELSE(v268,v269,DLC_Bool False);
                const !v273 = PLE(DLC_Int 0,v180);
                const !v274 = PLT(v180,DLC_Int 3);
                const *v276 = IF_THEN_ELSE(v273,v274,DLC_Bool False);
                const !v278 = IF_THEN_ELSE(v271,v276,DLC_Bool False);
                if v278 then {
                  const !v279 = SUB(DLC_Int 4,v180);
                  const !v280 = ADD(v227,v279);
                  const !v281 = MOD(v280,DLC_Int 3);
                  v266 = v281;
                   }
                else {
                  if v271 then {
                    v266 = DLC_Int 2;
                     }
                  else {
                    if v276 then {
                      v266 = DLC_Int 0;
                       }
                    else {
                      v266 = DLC_Int 1;
                       };
                     };
                   };
                const !v349 = ADD(DLC_Int 1,v75);
                {
                  v75 = v349,
                  v76 = v266}
                continue; } } } }
        let v356;
        const !v358 = PEQ(v76,DLC_Int 2);
        if v358 then {
          const !v359 = MUL(DLC_Int 2,v5);
          v356 = [v359,DLC_Int 0];
           }
        else {
          const !v361 = PEQ(v76,DLC_Int 0);
          if v361 then {
            const !v362 = MUL(DLC_Int 2,v5);
            v356 = [DLC_Int 0,v362];
             }
          else {
            v356 = [v5,v5];
             };
           };
        let v385;
        const !v387 = PLE(DLC_Int 0,v76);
        const !v388 = PLT(v76,DLC_Int 5);
        const !v390 = IF_THEN_ELSE(v387,v388,DLC_Bool False);
        claim(CT_Require)(v390);
        const !v392 = PEQ(v76,DLC_Int 0);
        if v392 then {
          v385 = DLC_Bytes "Bob wins";
           }
        else {
          const !v394 = PEQ(v76,DLC_Int 1);
          if v394 then {
            v385 = DLC_Bytes "Draw";
             }
          else {
            const !v396 = PEQ(v76,DLC_Int 2);
            if v396 then {
              v385 = DLC_Bytes "Alice wins";
               }
            else {
              const !v398 = PEQ(v76,DLC_Int 3);
              if v398 then {
                v385 = DLC_Bytes "Alice quits";
                 }
              else {
                v385 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        eff interact("B")."endsWith"(v385);
        
         } } },
  "O" = interact {
    };
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5,v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9,v10);
    claim(CT_Require)(v12);
    sendrecv join(v16) 2 ()()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 12 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0,v22);
          claim(CT_Require)(v24);
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5,v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()()
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 11 ()(){
            const !v52 = TXN_VALUE();
            const !v54 = PEQ(DLC_Int 0,v52);
            claim(CT_Require)(v54);
             } }){
        const !v47 = TXN_VALUE();
        const !v49 = PEQ(DLC_Int 0,v47);
        claim(CT_Require)(v49);
        loopvar {
          v75 = DLC_Int 0,
          v76 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v104 = PEQ(v76,DLC_Int 1);
           v104) }
        {
          sendrecv again(v7) 5 ()(v130)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 10 ()(){
                const !v136 = TXN_VALUE();
                const !v138 = PEQ(DLC_Int 0,v136);
                claim(CT_Require)(v138);
                 } }){
            const !v131 = TXN_VALUE();
            const !v133 = PEQ(DLC_Int 0,v131);
            claim(CT_Require)(v133);
            sendrecv again(v16) 6 ()(v180)
              .timeout(DLC_Int 10, {
                sendrecv again(v7) 9 ()(){
                  const !v186 = TXN_VALUE();
                  const !v188 = PEQ(DLC_Int 0,v186);
                  claim(CT_Require)(v188);
                   } }){
              const !v181 = TXN_VALUE();
              const !v183 = PEQ(DLC_Int 0,v181);
              claim(CT_Require)(v183);
              const !v210 = PLE(DLC_Int 0,v180);
              const !v211 = PLT(v180,DLC_Int 3);
              const !v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
              claim(CT_Require)(v213);
              sendrecv again(v7) 7 ()(v226, v227)
                .timeout(DLC_Int 10, {
                  sendrecv again(v16) 8 ()(){
                    const !v233 = TXN_VALUE();
                    const !v235 = PEQ(DLC_Int 0,v233);
                    claim(CT_Require)(v235);
                     } }){
                const !v228 = TXN_VALUE();
                const !v230 = PEQ(DLC_Int 0,v228);
                claim(CT_Require)(v230);
                const !v257 = digest(v226,v227);
                const !v259 = PEQ(v130,v257);
                claim(CT_Require)(v259);
                const !v261 = PLE(DLC_Int 0,v227);
                const !v262 = PLT(v227,DLC_Int 3);
                const !v264 = IF_THEN_ELSE(v261,v262,DLC_Bool False);
                claim(CT_Require)(v264);
                let v266;
                const !v268 = PLE(DLC_Int 0,v227);
                const !v269 = PLT(v227,DLC_Int 3);
                const *v271 = IF_THEN_ELSE(v268,v269,DLC_Bool False);
                const !v273 = PLE(DLC_Int 0,v180);
                const !v274 = PLT(v180,DLC_Int 3);
                const *v276 = IF_THEN_ELSE(v273,v274,DLC_Bool False);
                const !v278 = IF_THEN_ELSE(v271,v276,DLC_Bool False);
                if v278 then {
                  const !v279 = SUB(DLC_Int 4,v180);
                  const !v280 = ADD(v227,v279);
                  const !v281 = MOD(v280,DLC_Int 3);
                  v266 = v281;
                   }
                else {
                  if v271 then {
                    v266 = DLC_Int 2;
                     }
                  else {
                    if v276 then {
                      v266 = DLC_Int 0;
                       }
                    else {
                      v266 = DLC_Int 1;
                       };
                     };
                   };
                const !v349 = ADD(DLC_Int 1,v75);
                {
                  v75 = v349,
                  v76 = v266}
                continue; } } } }
        let v356;
        const !v358 = PEQ(v76,DLC_Int 2);
        if v358 then {
          const !v359 = MUL(DLC_Int 2,v5);
          v356 = [v359,DLC_Int 0];
           }
        else {
          const !v361 = PEQ(v76,DLC_Int 0);
          if v361 then {
            const !v362 = MUL(DLC_Int 2,v5);
            v356 = [DLC_Int 0,v362];
             }
          else {
            v356 = [v5,v5];
             };
           };
         } } }}

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
    [],
    {
      const !v47 = TXN_VALUE();
      const !v49 = PEQ(DLC_Int 0,v47);
      claim(CT_Require)(v49);
      (jump! 4 [v7, v5, v6, v16] {
        v75 = DLC_Int 0,
        v76 = DLC_Int 1}) } },
  4 = {
    loop!,
    [v7, v5, v6, v16],
    [v75, v76],
    {
      const !v104 = PEQ(v76,DLC_Int 1);
      
      if v104 then {
        (wait! [v7, v5, v6, v16, v75]) }
      else {
        let v356;
        const !v358 = PEQ(v76,DLC_Int 2);
        if v358 then {
          const !v359 = MUL(DLC_Int 2,v5);
          v356 = [v359,DLC_Int 0];
           }
        else {
          const !v361 = PEQ(v76,DLC_Int 0);
          if v361 then {
            const !v362 = MUL(DLC_Int 2,v5);
            v356 = [DLC_Int 0,v362];
             }
          else {
            v356 = [v5,v5];
             };
           };
        const !v363 = v356[0];
        const !v364 = v356[1];
        const !v365 = ADD(v6,v363);
        transfer.(v365).to(v7);
        transfer.(v364).to(v16);
        (halt! ) }; } },
  5 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 4,
    [v7, v5, v6, v16, v75],
    [v130],
    {
      const !v131 = TXN_VALUE();
      const !v133 = PEQ(DLC_Int 0,v131);
      claim(CT_Require)(v133);
      (wait! [v7, v5, v6, v16, v130, v75]) } },
  6 = {
    again(v16),
    (between [] [DLC_Int 10]),
    last = 5,
    [v7, v5, v6, v16, v130, v75],
    [v180],
    {
      const !v181 = TXN_VALUE();
      const !v183 = PEQ(DLC_Int 0,v181);
      claim(CT_Require)(v183);
      const !v210 = PLE(DLC_Int 0,v180);
      const !v211 = PLT(v180,DLC_Int 3);
      const !v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
      claim(CT_Require)(v213);
      (wait! [v7, v5, v6, v16, v130, v180, v75]) } },
  7 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 6,
    [v7, v5, v6, v16, v130, v180, v75],
    [v226, v227],
    {
      const !v228 = TXN_VALUE();
      const !v230 = PEQ(DLC_Int 0,v228);
      claim(CT_Require)(v230);
      const !v257 = digest(v226,v227);
      const !v259 = PEQ(v130,v257);
      claim(CT_Require)(v259);
      const !v261 = PLE(DLC_Int 0,v227);
      const !v262 = PLT(v227,DLC_Int 3);
      const !v264 = IF_THEN_ELSE(v261,v262,DLC_Bool False);
      claim(CT_Require)(v264);
      let v266;
      const !v268 = PLE(DLC_Int 0,v227);
      const !v269 = PLT(v227,DLC_Int 3);
      const *v271 = IF_THEN_ELSE(v268,v269,DLC_Bool False);
      const !v273 = PLE(DLC_Int 0,v180);
      const !v274 = PLT(v180,DLC_Int 3);
      const *v276 = IF_THEN_ELSE(v273,v274,DLC_Bool False);
      const !v278 = IF_THEN_ELSE(v271,v276,DLC_Bool False);
      if v278 then {
        const !v279 = SUB(DLC_Int 4,v180);
        const !v280 = ADD(v227,v279);
        const !v281 = MOD(v280,DLC_Int 3);
        v266 = v281;
         }
      else {
        if v271 then {
          v266 = DLC_Int 2;
           }
        else {
          if v276 then {
            v266 = DLC_Int 0;
             }
          else {
            v266 = DLC_Int 1;
             };
           };
         };
      const !v349 = ADD(DLC_Int 1,v75);
      (jump! 4 [v7, v5, v6, v16] {
        v75 = v349,
        v76 = v266}) } },
  8 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 6,
    [v7, v5, v6, v16, v130, v180, v75],
    [],
    {
      const !v233 = TXN_VALUE();
      const !v235 = PEQ(DLC_Int 0,v233);
      claim(CT_Require)(v235);
      const !v236 = BALANCE();
      transfer.(v236).to(v16);
      (halt! ) } },
  9 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 5,
    [v7, v5, v6, v16, v130, v75],
    [],
    {
      const !v186 = TXN_VALUE();
      const !v188 = PEQ(DLC_Int 0,v186);
      claim(CT_Require)(v188);
      const !v189 = BALANCE();
      transfer.(v189).to(v7);
      (halt! ) } },
  10 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 4,
    [v7, v5, v6, v16, v75],
    [],
    {
      const !v136 = TXN_VALUE();
      const !v138 = PEQ(DLC_Int 0,v136);
      claim(CT_Require)(v138);
      const !v139 = BALANCE();
      transfer.(v139).to(v16);
      (halt! ) } },
  11 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 2,
    [v7, v5, v6, v16],
    [],
    {
      const !v52 = TXN_VALUE();
      const !v54 = PEQ(DLC_Int 0,v52);
      claim(CT_Require)(v54);
      const !v55 = BALANCE();
      transfer.(v55).to(v16);
      (halt! ) } },
  12 = {
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