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
  const *v1 = interact("A")."getParams"();
  const *v2 = v1[0];
  const *v3 = v1[1];
  
  const !v7 = ADD(v2,v3);
  
  sendrecv join(v6) 1 (.publish((v2,v3), v7, ()))(v4, v5){
    const !v8 = ADD(v4,v5);
    const !v9 = TXN_VALUE();
    const !v10 = PEQ(v8,v9);
    claim(CT_Require)(v10);
    sendrecv join(v13) 2 ()().timeout(DLC_Int 10, {
      
      sendrecv again(v6) 10 (.publish((), DLC_Int 0, (v6, v4, v5)))(){
        const !v18 = TXN_VALUE();
        const !v19 = PEQ(DLC_Int 0,v18);
        claim(CT_Require)(v19);
        claim(CT_Require)(DLC_Bool True);
        eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const !v14 = TXN_VALUE();
      const !v15 = PEQ(v4,v14);
      claim(CT_Require)(v15);
      loopvar {
        v32 = DLC_Int 0,
        v33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const !v57 = PEQ(v33,DLC_Int 1);
         v57) }
      {
        let v60;
        const *v61 = interact("A")."getHand"();
        const *v62 = BYTES_EQ(v61,DLC_Bytes "ROCK");
        const *v63 = BYTES_EQ(v61,DLC_Bytes "PAPER");
        const !v64 = BYTES_EQ(v61,DLC_Bytes "SCISSORS");
        const !v66 = IF_THEN_ELSE(v62,DLC_Bool True,v63);
        const !v68 = IF_THEN_ELSE(v66,DLC_Bool True,v64);
        claim(CT_Assume)(v68);
        if v62 then {
          v60 = DLC_Int 0;
           }
        else {
          if v63 then {
            v60 = DLC_Int 1;
             }
          else {
            v60 = DLC_Int 2;
             };
           };
        const *v76 = interact("A")."random"();
        const !v77 = digest(v76,v60);
        eff interact("A")."commits"();
        
        
        sendrecv again(v6) 4 (.publish((v77), DLC_Int 0, (v6, v4, v5, v13, v32)))(v79).timeout(DLC_Int 10, {
          sendrecv again(v13) 9 ()(){
            const !v84 = TXN_VALUE();
            const !v85 = PEQ(DLC_Int 0,v84);
            claim(CT_Require)(v85);
            claim(CT_Require)(DLC_Bool True);
            eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
          const !v80 = TXN_VALUE();
          const !v81 = PEQ(DLC_Int 0,v80);
          claim(CT_Require)(v81);
          sendrecv again(v13) 5 ()(v116).timeout(DLC_Int 10, {
            
            sendrecv again(v6) 8 (.publish((), DLC_Int 0, (v6, v4, v5, v13, v79, v32)))(){
              const !v121 = TXN_VALUE();
              const !v122 = PEQ(DLC_Int 0,v121);
              claim(CT_Require)(v122);
              claim(CT_Require)(DLC_Bool True);
              eff interact("A")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
            const !v117 = TXN_VALUE();
            const !v118 = PEQ(DLC_Int 0,v117);
            claim(CT_Require)(v118);
            const !v136 = PLE(DLC_Int 0,v116);
            const !v137 = PLT(v116,DLC_Int 3);
            const !v139 = IF_THEN_ELSE(v136,v137,DLC_Bool False);
            claim(CT_Require)(v139);
            let v141;
            const !v143 = PLE(DLC_Int 0,v116);
            const !v144 = PLT(v116,DLC_Int 3);
            const !v146 = IF_THEN_ELSE(v143,v144,DLC_Bool False);
            claim(CT_Require)(v146);
            const !v147 = PEQ(v116,DLC_Int 0);
            if v147 then {
              v141 = DLC_Bytes "ROCK";
               }
            else {
              const !v148 = PEQ(v116,DLC_Int 1);
              if v148 then {
                v141 = DLC_Bytes "PAPER";
                 }
              else {
                v141 = DLC_Bytes "SCISSORS";
                 };
               };
            eff interact("A")."reveals"(v141);
            
            
            sendrecv again(v6) 6 (.publish((v76,v60), DLC_Int 0, (v6, v4, v5, v13, v79, v116, v32)))(v150, v151).timeout(DLC_Int 10, {
              sendrecv again(v13) 7 ()(){
                const !v156 = TXN_VALUE();
                const !v157 = PEQ(DLC_Int 0,v156);
                claim(CT_Require)(v157);
                claim(CT_Require)(DLC_Bool True);
                eff interact("A")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
              const !v152 = TXN_VALUE();
              const !v153 = PEQ(DLC_Int 0,v152);
              claim(CT_Require)(v153);
              const !v171 = digest(v150,v151);
              const !v172 = PEQ(v79,v171);
              claim(CT_Require)(v172);
              const !v174 = PLE(DLC_Int 0,v151);
              const !v175 = PLT(v151,DLC_Int 3);
              const !v177 = IF_THEN_ELSE(v174,v175,DLC_Bool False);
              claim(CT_Require)(v177);
              let v179;
              const !v181 = PLE(DLC_Int 0,v151);
              const !v182 = PLT(v151,DLC_Int 3);
              const *v184 = IF_THEN_ELSE(v181,v182,DLC_Bool False);
              const !v186 = PLE(DLC_Int 0,v116);
              const !v187 = PLT(v116,DLC_Int 3);
              const *v189 = IF_THEN_ELSE(v186,v187,DLC_Bool False);
              const !v191 = IF_THEN_ELSE(v184,v189,DLC_Bool False);
              if v191 then {
                const !v192 = SUB(DLC_Int 4,v116);
                const !v193 = ADD(v151,v192);
                const !v194 = MOD(v193,DLC_Int 3);
                v179 = v194;
                 }
              else {
                if v184 then {
                  v179 = DLC_Int 2;
                   }
                else {
                  if v189 then {
                    v179 = DLC_Int 0;
                     }
                  else {
                    v179 = DLC_Int 1;
                     };
                   };
                 };
              const !v252 = ADD(DLC_Int 1,v32);
              {
                v32 = v252,
                v33 = v179}
              continue; } } } }
      let v257;
      const !v258 = PEQ(v33,DLC_Int 2);
      if v258 then {
        const !v259 = MUL(DLC_Int 2,v4);
        v257 = [v259,DLC_Int 0];
         }
      else {
        const !v260 = PEQ(v33,DLC_Int 0);
        if v260 then {
          const !v261 = MUL(DLC_Int 2,v4);
          v257 = [DLC_Int 0,v261];
           }
        else {
          v257 = [v4,v4];
           };
         };
      let v268;
      const !v270 = PLE(DLC_Int 0,v33);
      const !v271 = PLT(v33,DLC_Int 5);
      const !v273 = IF_THEN_ELSE(v270,v271,DLC_Bool False);
      claim(CT_Require)(v273);
      const !v274 = PEQ(v33,DLC_Int 0);
      if v274 then {
        v268 = DLC_Bytes "Bob wins";
         }
      else {
        const !v275 = PEQ(v33,DLC_Int 1);
        if v275 then {
          v268 = DLC_Bytes "Draw";
           }
        else {
          const !v276 = PEQ(v33,DLC_Int 2);
          if v276 then {
            v268 = DLC_Bytes "Alice wins";
             }
          else {
            const !v277 = PEQ(v33,DLC_Int 3);
            if v277 then {
              v268 = DLC_Bytes "Alice quits";
               }
            else {
              v268 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      eff interact("A")."endsWith"(v268);
      
       } },
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null};
  sendrecv join(v6) 1 ()(v4, v5){
    const !v8 = ADD(v4,v5);
    const !v9 = TXN_VALUE();
    const !v10 = PEQ(v8,v9);
    claim(CT_Require)(v10);
    eff interact("B")."acceptParams"(v4,v5);
    
    
    sendrecv join(v13) 2 (.publish((), v4, (v6, v4, v5)))().timeout(DLC_Int 10, {
      sendrecv again(v6) 10 ()(){
        const !v18 = TXN_VALUE();
        const !v19 = PEQ(DLC_Int 0,v18);
        claim(CT_Require)(v19);
        claim(CT_Require)(DLC_Bool True);
        eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
        
         } }){
      const !v14 = TXN_VALUE();
      const !v15 = PEQ(v4,v14);
      claim(CT_Require)(v15);
      loopvar {
        v32 = DLC_Int 0,
        v33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const !v57 = PEQ(v33,DLC_Int 1);
         v57) }
      {
        sendrecv again(v6) 4 ()(v79).timeout(DLC_Int 10, {
          
          sendrecv again(v13) 9 (.publish((), DLC_Int 0, (v6, v4, v5, v13, v32)))(){
            const !v84 = TXN_VALUE();
            const !v85 = PEQ(DLC_Int 0,v84);
            claim(CT_Require)(v85);
            claim(CT_Require)(DLC_Bool True);
            eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
            
             } }){
          const !v80 = TXN_VALUE();
          const !v81 = PEQ(DLC_Int 0,v80);
          claim(CT_Require)(v81);
          let v100;
          const *v101 = interact("B")."getHand"();
          const *v102 = BYTES_EQ(v101,DLC_Bytes "ROCK");
          const *v103 = BYTES_EQ(v101,DLC_Bytes "PAPER");
          const !v104 = BYTES_EQ(v101,DLC_Bytes "SCISSORS");
          const !v106 = IF_THEN_ELSE(v102,DLC_Bool True,v103);
          const !v108 = IF_THEN_ELSE(v106,DLC_Bool True,v104);
          claim(CT_Assume)(v108);
          if v102 then {
            v100 = DLC_Int 0;
             }
          else {
            if v103 then {
              v100 = DLC_Int 1;
               }
            else {
              v100 = DLC_Int 2;
               };
             };
          eff interact("B")."shows"();
          
          
          sendrecv again(v13) 5 (.publish((v100), DLC_Int 0, (v6, v4, v5, v13, v79, v32)))(v116).timeout(DLC_Int 10, {
            sendrecv again(v6) 8 ()(){
              const !v121 = TXN_VALUE();
              const !v122 = PEQ(DLC_Int 0,v121);
              claim(CT_Require)(v122);
              claim(CT_Require)(DLC_Bool True);
              eff interact("B")."endsWith"(DLC_Bytes "Bob quits");
              
               } }){
            const !v117 = TXN_VALUE();
            const !v118 = PEQ(DLC_Int 0,v117);
            claim(CT_Require)(v118);
            const !v136 = PLE(DLC_Int 0,v116);
            const !v137 = PLT(v116,DLC_Int 3);
            const !v139 = IF_THEN_ELSE(v136,v137,DLC_Bool False);
            claim(CT_Require)(v139);
            sendrecv again(v6) 6 ()(v150, v151).timeout(DLC_Int 10, {
              
              sendrecv again(v13) 7 (.publish((), DLC_Int 0, (v6, v4, v5, v13, v79, v116, v32)))(){
                const !v156 = TXN_VALUE();
                const !v157 = PEQ(DLC_Int 0,v156);
                claim(CT_Require)(v157);
                claim(CT_Require)(DLC_Bool True);
                eff interact("B")."endsWith"(DLC_Bytes "Alice quits");
                
                 } }){
              const !v152 = TXN_VALUE();
              const !v153 = PEQ(DLC_Int 0,v152);
              claim(CT_Require)(v153);
              const !v171 = digest(v150,v151);
              const !v172 = PEQ(v79,v171);
              claim(CT_Require)(v172);
              const !v174 = PLE(DLC_Int 0,v151);
              const !v175 = PLT(v151,DLC_Int 3);
              const !v177 = IF_THEN_ELSE(v174,v175,DLC_Bool False);
              claim(CT_Require)(v177);
              let v179;
              const !v181 = PLE(DLC_Int 0,v151);
              const !v182 = PLT(v151,DLC_Int 3);
              const *v184 = IF_THEN_ELSE(v181,v182,DLC_Bool False);
              const !v186 = PLE(DLC_Int 0,v116);
              const !v187 = PLT(v116,DLC_Int 3);
              const *v189 = IF_THEN_ELSE(v186,v187,DLC_Bool False);
              const !v191 = IF_THEN_ELSE(v184,v189,DLC_Bool False);
              if v191 then {
                const !v192 = SUB(DLC_Int 4,v116);
                const !v193 = ADD(v151,v192);
                const !v194 = MOD(v193,DLC_Int 3);
                v179 = v194;
                 }
              else {
                if v184 then {
                  v179 = DLC_Int 2;
                   }
                else {
                  if v189 then {
                    v179 = DLC_Int 0;
                     }
                  else {
                    v179 = DLC_Int 1;
                     };
                   };
                 };
              const !v252 = ADD(DLC_Int 1,v32);
              {
                v32 = v252,
                v33 = v179}
              continue; } } } }
      let v257;
      const !v258 = PEQ(v33,DLC_Int 2);
      if v258 then {
        const !v259 = MUL(DLC_Int 2,v4);
        v257 = [v259,DLC_Int 0];
         }
      else {
        const !v260 = PEQ(v33,DLC_Int 0);
        if v260 then {
          const !v261 = MUL(DLC_Int 2,v4);
          v257 = [DLC_Int 0,v261];
           }
        else {
          v257 = [v4,v4];
           };
         };
      let v280;
      const !v282 = PLE(DLC_Int 0,v33);
      const !v283 = PLT(v33,DLC_Int 5);
      const !v285 = IF_THEN_ELSE(v282,v283,DLC_Bool False);
      claim(CT_Require)(v285);
      const !v286 = PEQ(v33,DLC_Int 0);
      if v286 then {
        v280 = DLC_Bytes "Bob wins";
         }
      else {
        const !v287 = PEQ(v33,DLC_Int 1);
        if v287 then {
          v280 = DLC_Bytes "Draw";
           }
        else {
          const !v288 = PEQ(v33,DLC_Int 2);
          if v288 then {
            v280 = DLC_Bytes "Alice wins";
             }
          else {
            const !v289 = PEQ(v33,DLC_Int 3);
            if v289 then {
              v280 = DLC_Bytes "Alice quits";
               }
            else {
              v280 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      eff interact("B")."endsWith"(v280);
      
       } },
  "O" = interact {
    };
  sendrecv join(v6) 1 ()(v4, v5){
    const !v8 = ADD(v4,v5);
    const !v9 = TXN_VALUE();
    const !v10 = PEQ(v8,v9);
    claim(CT_Require)(v10);
    sendrecv join(v13) 2 ()().timeout(DLC_Int 10, {
      sendrecv again(v6) 10 ()(){
        const !v18 = TXN_VALUE();
        const !v19 = PEQ(DLC_Int 0,v18);
        claim(CT_Require)(v19);
         } }){
      const !v14 = TXN_VALUE();
      const !v15 = PEQ(v4,v14);
      claim(CT_Require)(v15);
      loopvar {
        v32 = DLC_Int 0,
        v33 = DLC_Int 1};
      invariant{
        () }
      while{
        (begin const !v57 = PEQ(v33,DLC_Int 1);
         v57) }
      {
        sendrecv again(v6) 4 ()(v79).timeout(DLC_Int 10, {
          sendrecv again(v13) 9 ()(){
            const !v84 = TXN_VALUE();
            const !v85 = PEQ(DLC_Int 0,v84);
            claim(CT_Require)(v85);
             } }){
          const !v80 = TXN_VALUE();
          const !v81 = PEQ(DLC_Int 0,v80);
          claim(CT_Require)(v81);
          sendrecv again(v13) 5 ()(v116).timeout(DLC_Int 10, {
            sendrecv again(v6) 8 ()(){
              const !v121 = TXN_VALUE();
              const !v122 = PEQ(DLC_Int 0,v121);
              claim(CT_Require)(v122);
               } }){
            const !v117 = TXN_VALUE();
            const !v118 = PEQ(DLC_Int 0,v117);
            claim(CT_Require)(v118);
            const !v136 = PLE(DLC_Int 0,v116);
            const !v137 = PLT(v116,DLC_Int 3);
            const !v139 = IF_THEN_ELSE(v136,v137,DLC_Bool False);
            claim(CT_Require)(v139);
            sendrecv again(v6) 6 ()(v150, v151).timeout(DLC_Int 10, {
              sendrecv again(v13) 7 ()(){
                const !v156 = TXN_VALUE();
                const !v157 = PEQ(DLC_Int 0,v156);
                claim(CT_Require)(v157);
                 } }){
              const !v152 = TXN_VALUE();
              const !v153 = PEQ(DLC_Int 0,v152);
              claim(CT_Require)(v153);
              const !v171 = digest(v150,v151);
              const !v172 = PEQ(v79,v171);
              claim(CT_Require)(v172);
              const !v174 = PLE(DLC_Int 0,v151);
              const !v175 = PLT(v151,DLC_Int 3);
              const !v177 = IF_THEN_ELSE(v174,v175,DLC_Bool False);
              claim(CT_Require)(v177);
              let v179;
              const !v181 = PLE(DLC_Int 0,v151);
              const !v182 = PLT(v151,DLC_Int 3);
              const *v184 = IF_THEN_ELSE(v181,v182,DLC_Bool False);
              const !v186 = PLE(DLC_Int 0,v116);
              const !v187 = PLT(v116,DLC_Int 3);
              const *v189 = IF_THEN_ELSE(v186,v187,DLC_Bool False);
              const !v191 = IF_THEN_ELSE(v184,v189,DLC_Bool False);
              if v191 then {
                const !v192 = SUB(DLC_Int 4,v116);
                const !v193 = ADD(v151,v192);
                const !v194 = MOD(v193,DLC_Int 3);
                v179 = v194;
                 }
              else {
                if v184 then {
                  v179 = DLC_Int 2;
                   }
                else {
                  if v189 then {
                    v179 = DLC_Int 0;
                     }
                  else {
                    v179 = DLC_Int 1;
                     };
                   };
                 };
              const !v252 = ADD(DLC_Int 1,v32);
              {
                v32 = v252,
                v33 = v179}
              continue; } } } }
      let v257;
      const !v258 = PEQ(v33,DLC_Int 2);
      if v258 then {
        const !v259 = MUL(DLC_Int 2,v4);
        v257 = [v259,DLC_Int 0];
         }
      else {
        const !v260 = PEQ(v33,DLC_Int 0);
        if v260 then {
          const !v261 = MUL(DLC_Int 2,v4);
          v257 = [DLC_Int 0,v261];
           }
        else {
          v257 = [v4,v4];
           };
         };
       } }}

{
  1 = {
    join(v6),
    (between [] []),
    last = 0,
    [],
    [v4, v5],
    {
      const !v8 = ADD(v4,v5);
      const !v9 = TXN_VALUE();
      const !v10 = PEQ(v8,v9);
      claim(CT_Require)(v10);
      (wait! [v6, v4, v5]) } },
  2 = {
    join(v13),
    (between [] [DLC_Int 10]),
    last = 1,
    [v6, v4, v5],
    [],
    {
      const !v14 = TXN_VALUE();
      const !v15 = PEQ(v4,v14);
      claim(CT_Require)(v15);
      (jump! 3 [v6, v4, v5, v13] {
        v32 = DLC_Int 0,
        v33 = DLC_Int 1}) } },
  3 = {
    loop!,
    [v6, v4, v5, v13],
    [v32, v33],
    {
      const !v57 = PEQ(v33,DLC_Int 1);
      
      if v57 then {
        (wait! [v6, v4, v5, v13, v32]) }
      else {
        let v257;
        const !v258 = PEQ(v33,DLC_Int 2);
        if v258 then {
          const !v259 = MUL(DLC_Int 2,v4);
          v257 = [v259,DLC_Int 0];
           }
        else {
          const !v260 = PEQ(v33,DLC_Int 0);
          if v260 then {
            const !v261 = MUL(DLC_Int 2,v4);
            v257 = [DLC_Int 0,v261];
             }
          else {
            v257 = [v4,v4];
             };
           };
        const !v262 = v257[0];
        const !v263 = v257[1];
        const !v264 = ADD(v5,v262);
        transfer.(v264).to(v6);
        transfer.(v263).to(v13);
        (halt! ) }; } },
  4 = {
    again(v6),
    (between [] [DLC_Int 10]),
    last = 3,
    [v6, v4, v5, v13, v32],
    [v79],
    {
      const !v80 = TXN_VALUE();
      const !v81 = PEQ(DLC_Int 0,v80);
      claim(CT_Require)(v81);
      (wait! [v6, v4, v5, v13, v79, v32]) } },
  5 = {
    again(v13),
    (between [] [DLC_Int 10]),
    last = 4,
    [v6, v4, v5, v13, v79, v32],
    [v116],
    {
      const !v117 = TXN_VALUE();
      const !v118 = PEQ(DLC_Int 0,v117);
      claim(CT_Require)(v118);
      const !v136 = PLE(DLC_Int 0,v116);
      const !v137 = PLT(v116,DLC_Int 3);
      const !v139 = IF_THEN_ELSE(v136,v137,DLC_Bool False);
      claim(CT_Require)(v139);
      (wait! [v6, v4, v5, v13, v79, v116, v32]) } },
  6 = {
    again(v6),
    (between [] [DLC_Int 10]),
    last = 5,
    [v6, v4, v5, v13, v79, v116, v32],
    [v150, v151],
    {
      const !v152 = TXN_VALUE();
      const !v153 = PEQ(DLC_Int 0,v152);
      claim(CT_Require)(v153);
      const !v171 = digest(v150,v151);
      const !v172 = PEQ(v79,v171);
      claim(CT_Require)(v172);
      const !v174 = PLE(DLC_Int 0,v151);
      const !v175 = PLT(v151,DLC_Int 3);
      const !v177 = IF_THEN_ELSE(v174,v175,DLC_Bool False);
      claim(CT_Require)(v177);
      let v179;
      const !v181 = PLE(DLC_Int 0,v151);
      const !v182 = PLT(v151,DLC_Int 3);
      const *v184 = IF_THEN_ELSE(v181,v182,DLC_Bool False);
      const !v186 = PLE(DLC_Int 0,v116);
      const !v187 = PLT(v116,DLC_Int 3);
      const *v189 = IF_THEN_ELSE(v186,v187,DLC_Bool False);
      const !v191 = IF_THEN_ELSE(v184,v189,DLC_Bool False);
      if v191 then {
        const !v192 = SUB(DLC_Int 4,v116);
        const !v193 = ADD(v151,v192);
        const !v194 = MOD(v193,DLC_Int 3);
        v179 = v194;
         }
      else {
        if v184 then {
          v179 = DLC_Int 2;
           }
        else {
          if v189 then {
            v179 = DLC_Int 0;
             }
          else {
            v179 = DLC_Int 1;
             };
           };
         };
      const !v252 = ADD(DLC_Int 1,v32);
      (jump! 3 [v6, v4, v5, v13] {
        v32 = v252,
        v33 = v179}) } },
  7 = {
    again(v13),
    (between [DLC_Int 10] []),
    last = 5,
    [v6, v4, v5, v13, v79, v116, v32],
    [],
    {
      const !v156 = TXN_VALUE();
      const !v157 = PEQ(DLC_Int 0,v156);
      claim(CT_Require)(v157);
      const !v158 = BALANCE();
      transfer.(v158).to(v13);
      (halt! ) } },
  8 = {
    again(v6),
    (between [DLC_Int 10] []),
    last = 4,
    [v6, v4, v5, v13, v79, v32],
    [],
    {
      const !v121 = TXN_VALUE();
      const !v122 = PEQ(DLC_Int 0,v121);
      claim(CT_Require)(v122);
      const !v123 = BALANCE();
      transfer.(v123).to(v6);
      (halt! ) } },
  9 = {
    again(v13),
    (between [DLC_Int 10] []),
    last = 3,
    [v6, v4, v5, v13, v32],
    [],
    {
      const !v84 = TXN_VALUE();
      const !v85 = PEQ(DLC_Int 0,v84);
      claim(CT_Require)(v85);
      const !v86 = BALANCE();
      transfer.(v86).to(v13);
      (halt! ) } },
  10 = {
    again(v6),
    (between [DLC_Int 10] []),
    last = 1,
    [v6, v4, v5],
    [],
    {
      const !v18 = TXN_VALUE();
      const !v19 = PEQ(DLC_Int 0,v18);
      claim(CT_Require)(v19);
      const !v20 = BALANCE();
      transfer.(v20).to(v6);
      (halt! ) } }}