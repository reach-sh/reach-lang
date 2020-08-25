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
  
  const !v8 = ADD(v3, v4);
  
  sendrecv join(v7) 1 (.publish((v3, v4), v8, ()))(v5, v6){
    const !v9 = ADD(v5, v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9, v10);
    claim(CT_Require)(v12);
    sendrecv join(v16) 2 ()()
      .timeout(DLC_Int 10, {
        
        sendrecv again(v7) 12 (.publish((), DLC_Int 0, (v7, v5, v6)))(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0, v22);
          claim(CT_Require)(v24);
          eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5, v17);
      claim(CT_Require)(v19);
      eff interact("A")."partnerIs"(v16);
      
      
      sendrecv again(v7) 3 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))()
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 11 ()(){
            const !v50 = TXN_VALUE();
            const !v52 = PEQ(DLC_Int 0, v50);
            claim(CT_Require)(v52);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v45 = TXN_VALUE();
        const !v47 = PEQ(DLC_Int 0, v45);
        claim(CT_Require)(v47);
        loopvar {
          v71 = DLC_Int 0,
          v72 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v96 = PEQ(v72, DLC_Int 1);
           v96) }
        {
          let v100;
          const *v101 = interact("A")."getHand"();
          const *v103 = BYTES_EQ(v101, DLC_Bytes "ROCK");
          const *v105 = BYTES_EQ(v101, DLC_Bytes "PAPER");
          const !v107 = BYTES_EQ(v101, DLC_Bytes "SCISSORS");
          const !v108 = IF_THEN_ELSE(v103, DLC_Bool True, v105);
          const !v109 = IF_THEN_ELSE(v108, DLC_Bool True, v107);
          claim(CT_Assume)(v109);
          if v103 then {
            v100 = DLC_Int 0;
             }
          else {
            if v105 then {
              v100 = DLC_Int 1;
               }
            else {
              v100 = DLC_Int 2;
               };
             };
          const *v116 = interact("A")."random"();
          const !v117 = digest(v116, v100);
          eff interact("A")."commits"();
          
          
          sendrecv again(v7) 5 (.publish((v117), DLC_Int 0, (v7, v5, v6, v16, v71)))(v119)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 10 ()(){
                const !v125 = TXN_VALUE();
                const !v127 = PEQ(DLC_Int 0, v125);
                claim(CT_Require)(v127);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v120 = TXN_VALUE();
            const !v122 = PEQ(DLC_Int 0, v120);
            claim(CT_Require)(v122);
            sendrecv again(v16) 6 ()(v164)
              .timeout(DLC_Int 10, {
                
                sendrecv again(v7) 9 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v119, v71)))(){
                  const !v170 = TXN_VALUE();
                  const !v172 = PEQ(DLC_Int 0, v170);
                  claim(CT_Require)(v172);
                  eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
                  
                   } }){
              const !v165 = TXN_VALUE();
              const !v167 = PEQ(DLC_Int 0, v165);
              claim(CT_Require)(v167);
              const !v192 = PLE(DLC_Int 0, v164);
              const !v193 = PLT(v164, DLC_Int 3);
              const !v194 = IF_THEN_ELSE(v192, v193, DLC_Bool False);
              claim(CT_Require)(v194);
              let v196;
              const !v202 = PEQ(v164, DLC_Int 0);
              if v202 then {
                v196 = DLC_Bytes "ROCK";
                 }
              else {
                const !v204 = PEQ(v164, DLC_Int 1);
                if v204 then {
                  v196 = DLC_Bytes "PAPER";
                   }
                else {
                  v196 = DLC_Bytes "SCISSORS";
                   };
                 };
              eff interact("A")."reveals"(v196);
              
              
              sendrecv again(v7) 7 (.publish((v116, v100), DLC_Int 0, (v7, v5, v6, v16, v119, v164, v71)))(v206, v207)
                .timeout(DLC_Int 10, {
                  sendrecv again(v16) 8 ()(){
                    const !v213 = TXN_VALUE();
                    const !v215 = PEQ(DLC_Int 0, v213);
                    claim(CT_Require)(v215);
                    eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                    
                     } }){
                const !v208 = TXN_VALUE();
                const !v210 = PEQ(DLC_Int 0, v208);
                claim(CT_Require)(v210);
                const !v235 = digest(v206, v207);
                const !v237 = PEQ(v119, v235);
                claim(CT_Require)(v237);
                const !v239 = PLE(DLC_Int 0, v207);
                const !v240 = PLT(v207, DLC_Int 3);
                const !v241 = IF_THEN_ELSE(v239, v240, DLC_Bool False);
                claim(CT_Require)(v241);
                let v243;
                const !v245 = PLE(DLC_Int 0, v207);
                const !v246 = PLT(v207, DLC_Int 3);
                const *v247 = IF_THEN_ELSE(v245, v246, DLC_Bool False);
                const !v249 = PLE(DLC_Int 0, v164);
                const !v250 = PLT(v164, DLC_Int 3);
                const *v251 = IF_THEN_ELSE(v249, v250, DLC_Bool False);
                const !v252 = IF_THEN_ELSE(v247, v251, DLC_Bool False);
                if v252 then {
                  const !v253 = SUB(DLC_Int 4, v164);
                  const !v254 = ADD(v207, v253);
                  const !v255 = MOD(v254, DLC_Int 3);
                  v243 = v255;
                   }
                else {
                  if v247 then {
                    v243 = DLC_Int 2;
                     }
                  else {
                    if v251 then {
                      v243 = DLC_Int 0;
                       }
                    else {
                      v243 = DLC_Int 1;
                       };
                     };
                   };
                const !v312 = ADD(DLC_Int 1, v71);
                {
                  v71 = v312,
                  v72 = v243}
                continue; } } } }
        let v319;
        const !v321 = PEQ(v72, DLC_Int 2);
        if v321 then {
          const !v322 = MUL(DLC_Int 2, v5);
          v319 = [v322, DLC_Int 0];
           }
        else {
          const !v324 = PEQ(v72, DLC_Int 0);
          if v324 then {
            const !v325 = MUL(DLC_Int 2, v5);
            v319 = [DLC_Int 0, v325];
             }
          else {
            v319 = [v5, v5];
             };
           };
        let v332;
        const !v338 = PEQ(v72, DLC_Int 0);
        if v338 then {
          v332 = DLC_Bytes "Bob wins";
           }
        else {
          const !v340 = PEQ(v72, DLC_Int 1);
          if v340 then {
            v332 = DLC_Bytes "Draw";
             }
          else {
            const !v342 = PEQ(v72, DLC_Int 2);
            if v342 then {
              v332 = DLC_Bytes "Alice wins";
               }
            else {
              const !v344 = PEQ(v72, DLC_Int 3);
              if v344 then {
                v332 = DLC_Bytes "Alice quits";
                 }
              else {
                v332 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        eff interact("A")."endsWith"(v332);
        
         } } },
  "B" = interact {
    acceptParams = Fun([UInt256, UInt256], Null),
    endsWith = Fun([Bytes], Null),
    getHand = Fun([], Bytes),
    partnerIs = Fun([Address], Null),
    random = Fun([], UInt256),
    shows = Fun([], Null)};
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5, v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9, v10);
    claim(CT_Require)(v12);
    eff interact("B")."partnerIs"(v7);
    eff interact("B")."acceptParams"(v5, v6);
    
    
    sendrecv join(v16) 2 (.publish((), v5, (v7, v5, v6)))()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 12 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0, v22);
          claim(CT_Require)(v24);
          eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
          
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5, v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()()
        .timeout(DLC_Int 10, {
          
          sendrecv again(v16) 11 (.publish((), DLC_Int 0, (v7, v5, v6, v16)))(){
            const !v50 = TXN_VALUE();
            const !v52 = PEQ(DLC_Int 0, v50);
            claim(CT_Require)(v52);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
        const !v45 = TXN_VALUE();
        const !v47 = PEQ(DLC_Int 0, v45);
        claim(CT_Require)(v47);
        loopvar {
          v71 = DLC_Int 0,
          v72 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v96 = PEQ(v72, DLC_Int 1);
           v96) }
        {
          sendrecv again(v7) 5 ()(v119)
            .timeout(DLC_Int 10, {
              
              sendrecv again(v16) 10 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v71)))(){
                const !v125 = TXN_VALUE();
                const !v127 = PEQ(DLC_Int 0, v125);
                claim(CT_Require)(v127);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
            const !v120 = TXN_VALUE();
            const !v122 = PEQ(DLC_Int 0, v120);
            claim(CT_Require)(v122);
            let v148;
            const *v149 = interact("B")."getHand"();
            const *v151 = BYTES_EQ(v149, DLC_Bytes "ROCK");
            const *v153 = BYTES_EQ(v149, DLC_Bytes "PAPER");
            const !v155 = BYTES_EQ(v149, DLC_Bytes "SCISSORS");
            const !v156 = IF_THEN_ELSE(v151, DLC_Bool True, v153);
            const !v157 = IF_THEN_ELSE(v156, DLC_Bool True, v155);
            claim(CT_Assume)(v157);
            if v151 then {
              v148 = DLC_Int 0;
               }
            else {
              if v153 then {
                v148 = DLC_Int 1;
                 }
              else {
                v148 = DLC_Int 2;
                 };
               };
            eff interact("B")."shows"();
            
            
            sendrecv again(v16) 6 (.publish((v148), DLC_Int 0, (v7, v5, v6, v16, v119, v71)))(v164)
              .timeout(DLC_Int 10, {
                sendrecv again(v7) 9 ()(){
                  const !v170 = TXN_VALUE();
                  const !v172 = PEQ(DLC_Int 0, v170);
                  claim(CT_Require)(v172);
                  eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
                  
                   } }){
              const !v165 = TXN_VALUE();
              const !v167 = PEQ(DLC_Int 0, v165);
              claim(CT_Require)(v167);
              const !v192 = PLE(DLC_Int 0, v164);
              const !v193 = PLT(v164, DLC_Int 3);
              const !v194 = IF_THEN_ELSE(v192, v193, DLC_Bool False);
              claim(CT_Require)(v194);
              sendrecv again(v7) 7 ()(v206, v207)
                .timeout(DLC_Int 10, {
                  
                  sendrecv again(v16) 8 (.publish((), DLC_Int 0, (v7, v5, v6, v16, v119, v164, v71)))(){
                    const !v213 = TXN_VALUE();
                    const !v215 = PEQ(DLC_Int 0, v213);
                    claim(CT_Require)(v215);
                    eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                    
                     } }){
                const !v208 = TXN_VALUE();
                const !v210 = PEQ(DLC_Int 0, v208);
                claim(CT_Require)(v210);
                const !v235 = digest(v206, v207);
                const !v237 = PEQ(v119, v235);
                claim(CT_Require)(v237);
                const !v239 = PLE(DLC_Int 0, v207);
                const !v240 = PLT(v207, DLC_Int 3);
                const !v241 = IF_THEN_ELSE(v239, v240, DLC_Bool False);
                claim(CT_Require)(v241);
                let v243;
                const !v245 = PLE(DLC_Int 0, v207);
                const !v246 = PLT(v207, DLC_Int 3);
                const *v247 = IF_THEN_ELSE(v245, v246, DLC_Bool False);
                const !v249 = PLE(DLC_Int 0, v164);
                const !v250 = PLT(v164, DLC_Int 3);
                const *v251 = IF_THEN_ELSE(v249, v250, DLC_Bool False);
                const !v252 = IF_THEN_ELSE(v247, v251, DLC_Bool False);
                if v252 then {
                  const !v253 = SUB(DLC_Int 4, v164);
                  const !v254 = ADD(v207, v253);
                  const !v255 = MOD(v254, DLC_Int 3);
                  v243 = v255;
                   }
                else {
                  if v247 then {
                    v243 = DLC_Int 2;
                     }
                  else {
                    if v251 then {
                      v243 = DLC_Int 0;
                       }
                    else {
                      v243 = DLC_Int 1;
                       };
                     };
                   };
                const !v312 = ADD(DLC_Int 1, v71);
                {
                  v71 = v312,
                  v72 = v243}
                continue; } } } }
        let v319;
        const !v321 = PEQ(v72, DLC_Int 2);
        if v321 then {
          const !v322 = MUL(DLC_Int 2, v5);
          v319 = [v322, DLC_Int 0];
           }
        else {
          const !v324 = PEQ(v72, DLC_Int 0);
          if v324 then {
            const !v325 = MUL(DLC_Int 2, v5);
            v319 = [DLC_Int 0, v325];
             }
          else {
            v319 = [v5, v5];
             };
           };
        let v347;
        const !v353 = PEQ(v72, DLC_Int 0);
        if v353 then {
          v347 = DLC_Bytes "Bob wins";
           }
        else {
          const !v355 = PEQ(v72, DLC_Int 1);
          if v355 then {
            v347 = DLC_Bytes "Draw";
             }
          else {
            const !v357 = PEQ(v72, DLC_Int 2);
            if v357 then {
              v347 = DLC_Bytes "Alice wins";
               }
            else {
              const !v359 = PEQ(v72, DLC_Int 3);
              if v359 then {
                v347 = DLC_Bytes "Alice quits";
                 }
              else {
                v347 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        eff interact("B")."endsWith"(v347);
        
         } } },
  "O" = interact {
    };
  sendrecv join(v7) 1 ()(v5, v6){
    const !v9 = ADD(v5, v6);
    const !v10 = TXN_VALUE();
    const !v12 = PEQ(v9, v10);
    claim(CT_Require)(v12);
    sendrecv join(v16) 2 ()()
      .timeout(DLC_Int 10, {
        sendrecv again(v7) 12 ()(){
          const !v22 = TXN_VALUE();
          const !v24 = PEQ(DLC_Int 0, v22);
          claim(CT_Require)(v24);
           } }){
      const !v17 = TXN_VALUE();
      const !v19 = PEQ(v5, v17);
      claim(CT_Require)(v19);
      sendrecv again(v7) 3 ()()
        .timeout(DLC_Int 10, {
          sendrecv again(v16) 11 ()(){
            const !v50 = TXN_VALUE();
            const !v52 = PEQ(DLC_Int 0, v50);
            claim(CT_Require)(v52);
             } }){
        const !v45 = TXN_VALUE();
        const !v47 = PEQ(DLC_Int 0, v45);
        claim(CT_Require)(v47);
        loopvar {
          v71 = DLC_Int 0,
          v72 = DLC_Int 1};
        invariant{
          () }
        while{
          (begin const !v96 = PEQ(v72, DLC_Int 1);
           v96) }
        {
          sendrecv again(v7) 5 ()(v119)
            .timeout(DLC_Int 10, {
              sendrecv again(v16) 10 ()(){
                const !v125 = TXN_VALUE();
                const !v127 = PEQ(DLC_Int 0, v125);
                claim(CT_Require)(v127);
                 } }){
            const !v120 = TXN_VALUE();
            const !v122 = PEQ(DLC_Int 0, v120);
            claim(CT_Require)(v122);
            sendrecv again(v16) 6 ()(v164)
              .timeout(DLC_Int 10, {
                sendrecv again(v7) 9 ()(){
                  const !v170 = TXN_VALUE();
                  const !v172 = PEQ(DLC_Int 0, v170);
                  claim(CT_Require)(v172);
                   } }){
              const !v165 = TXN_VALUE();
              const !v167 = PEQ(DLC_Int 0, v165);
              claim(CT_Require)(v167);
              const !v192 = PLE(DLC_Int 0, v164);
              const !v193 = PLT(v164, DLC_Int 3);
              const !v194 = IF_THEN_ELSE(v192, v193, DLC_Bool False);
              claim(CT_Require)(v194);
              sendrecv again(v7) 7 ()(v206, v207)
                .timeout(DLC_Int 10, {
                  sendrecv again(v16) 8 ()(){
                    const !v213 = TXN_VALUE();
                    const !v215 = PEQ(DLC_Int 0, v213);
                    claim(CT_Require)(v215);
                     } }){
                const !v208 = TXN_VALUE();
                const !v210 = PEQ(DLC_Int 0, v208);
                claim(CT_Require)(v210);
                const !v235 = digest(v206, v207);
                const !v237 = PEQ(v119, v235);
                claim(CT_Require)(v237);
                const !v239 = PLE(DLC_Int 0, v207);
                const !v240 = PLT(v207, DLC_Int 3);
                const !v241 = IF_THEN_ELSE(v239, v240, DLC_Bool False);
                claim(CT_Require)(v241);
                let v243;
                const !v245 = PLE(DLC_Int 0, v207);
                const !v246 = PLT(v207, DLC_Int 3);
                const *v247 = IF_THEN_ELSE(v245, v246, DLC_Bool False);
                const !v249 = PLE(DLC_Int 0, v164);
                const !v250 = PLT(v164, DLC_Int 3);
                const *v251 = IF_THEN_ELSE(v249, v250, DLC_Bool False);
                const !v252 = IF_THEN_ELSE(v247, v251, DLC_Bool False);
                if v252 then {
                  const !v253 = SUB(DLC_Int 4, v164);
                  const !v254 = ADD(v207, v253);
                  const !v255 = MOD(v254, DLC_Int 3);
                  v243 = v255;
                   }
                else {
                  if v247 then {
                    v243 = DLC_Int 2;
                     }
                  else {
                    if v251 then {
                      v243 = DLC_Int 0;
                       }
                    else {
                      v243 = DLC_Int 1;
                       };
                     };
                   };
                const !v312 = ADD(DLC_Int 1, v71);
                {
                  v71 = v312,
                  v72 = v243}
                continue; } } } }
        let v319;
        const !v321 = PEQ(v72, DLC_Int 2);
        if v321 then {
          const !v322 = MUL(DLC_Int 2, v5);
          v319 = [v322, DLC_Int 0];
           }
        else {
          const !v324 = PEQ(v72, DLC_Int 0);
          if v324 then {
            const !v325 = MUL(DLC_Int 2, v5);
            v319 = [DLC_Int 0, v325];
             }
          else {
            v319 = [v5, v5];
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
      const !v9 = ADD(v5, v6);
      const !v10 = TXN_VALUE();
      const !v12 = PEQ(v9, v10);
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
      const !v19 = PEQ(v5, v17);
      claim(CT_Require)(v19);
      (wait! [v7, v5, v6, v16]) } },
  3 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 2,
    [v7, v5, v6, v16],
    [],
    {
      const !v45 = TXN_VALUE();
      const !v47 = PEQ(DLC_Int 0, v45);
      claim(CT_Require)(v47);
      (jump! 4 [v7, v5, v6, v16] {
        v71 = DLC_Int 0,
        v72 = DLC_Int 1}) } },
  4 = {
    loop!,
    [v7, v5, v6, v16],
    [v71, v72],
    {
      const !v96 = PEQ(v72, DLC_Int 1);
      
      if v96 then {
        (wait! [v7, v5, v6, v16, v71]) }
      else {
        let v319;
        const !v321 = PEQ(v72, DLC_Int 2);
        if v321 then {
          const !v322 = MUL(DLC_Int 2, v5);
          v319 = [v322, DLC_Int 0];
           }
        else {
          const !v324 = PEQ(v72, DLC_Int 0);
          if v324 then {
            const !v325 = MUL(DLC_Int 2, v5);
            v319 = [DLC_Int 0, v325];
             }
          else {
            v319 = [v5, v5];
             };
           };
        const !v326 = v319[0];
        const !v327 = v319[1];
        const !v328 = ADD(v6, v326);
        transfer.(v328).to(v7);
        transfer.(v327).to(v16);
        (halt! ) }; } },
  5 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 4,
    [v7, v5, v6, v16, v71],
    [v119],
    {
      const !v120 = TXN_VALUE();
      const !v122 = PEQ(DLC_Int 0, v120);
      claim(CT_Require)(v122);
      (wait! [v7, v5, v6, v16, v119, v71]) } },
  6 = {
    again(v16),
    (between [] [DLC_Int 10]),
    last = 5,
    [v7, v5, v6, v16, v119, v71],
    [v164],
    {
      const !v165 = TXN_VALUE();
      const !v167 = PEQ(DLC_Int 0, v165);
      claim(CT_Require)(v167);
      const !v192 = PLE(DLC_Int 0, v164);
      const !v193 = PLT(v164, DLC_Int 3);
      const !v194 = IF_THEN_ELSE(v192, v193, DLC_Bool False);
      claim(CT_Require)(v194);
      (wait! [v7, v5, v6, v16, v119, v164, v71]) } },
  7 = {
    again(v7),
    (between [] [DLC_Int 10]),
    last = 6,
    [v7, v5, v6, v16, v119, v164, v71],
    [v206, v207],
    {
      const !v208 = TXN_VALUE();
      const !v210 = PEQ(DLC_Int 0, v208);
      claim(CT_Require)(v210);
      const !v235 = digest(v206, v207);
      const !v237 = PEQ(v119, v235);
      claim(CT_Require)(v237);
      const !v239 = PLE(DLC_Int 0, v207);
      const !v240 = PLT(v207, DLC_Int 3);
      const !v241 = IF_THEN_ELSE(v239, v240, DLC_Bool False);
      claim(CT_Require)(v241);
      let v243;
      const !v245 = PLE(DLC_Int 0, v207);
      const !v246 = PLT(v207, DLC_Int 3);
      const *v247 = IF_THEN_ELSE(v245, v246, DLC_Bool False);
      const !v249 = PLE(DLC_Int 0, v164);
      const !v250 = PLT(v164, DLC_Int 3);
      const *v251 = IF_THEN_ELSE(v249, v250, DLC_Bool False);
      const !v252 = IF_THEN_ELSE(v247, v251, DLC_Bool False);
      if v252 then {
        const !v253 = SUB(DLC_Int 4, v164);
        const !v254 = ADD(v207, v253);
        const !v255 = MOD(v254, DLC_Int 3);
        v243 = v255;
         }
      else {
        if v247 then {
          v243 = DLC_Int 2;
           }
        else {
          if v251 then {
            v243 = DLC_Int 0;
             }
          else {
            v243 = DLC_Int 1;
             };
           };
         };
      const !v312 = ADD(DLC_Int 1, v71);
      (jump! 4 [v7, v5, v6, v16] {
        v71 = v312,
        v72 = v243}) } },
  8 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 6,
    [v7, v5, v6, v16, v119, v164, v71],
    [],
    {
      const !v213 = TXN_VALUE();
      const !v215 = PEQ(DLC_Int 0, v213);
      claim(CT_Require)(v215);
      const !v216 = BALANCE();
      transfer.(v216).to(v16);
      (halt! ) } },
  9 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 5,
    [v7, v5, v6, v16, v119, v71],
    [],
    {
      const !v170 = TXN_VALUE();
      const !v172 = PEQ(DLC_Int 0, v170);
      claim(CT_Require)(v172);
      const !v173 = BALANCE();
      transfer.(v173).to(v7);
      (halt! ) } },
  10 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 4,
    [v7, v5, v6, v16, v71],
    [],
    {
      const !v125 = TXN_VALUE();
      const !v127 = PEQ(DLC_Int 0, v125);
      claim(CT_Require)(v127);
      const !v128 = BALANCE();
      transfer.(v128).to(v16);
      (halt! ) } },
  11 = {
    again(v16),
    (between [DLC_Int 10] []),
    last = 2,
    [v7, v5, v6, v16],
    [],
    {
      const !v50 = TXN_VALUE();
      const !v52 = PEQ(DLC_Int 0, v50);
      claim(CT_Require)(v52);
      const !v53 = BALANCE();
      transfer.(v53).to(v16);
      (halt! ) } },
  12 = {
    again(v7),
    (between [DLC_Int 10] []),
    last = 1,
    [v7, v5, v6],
    [],
    {
      const !v22 = TXN_VALUE();
      const !v24 = PEQ(DLC_Int 0, v22);
      claim(CT_Require)(v24);
      const !v25 = BALANCE();
      transfer.(v25).to(v7);
      (halt! ) } }}