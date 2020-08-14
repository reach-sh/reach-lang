#lang ll
parts {
  "A" = interact {
    commits = T_Fun [] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    getParams = T_Fun [] (T_Tuple [T_UInt256,T_UInt256]),
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    reveals = T_Fun [T_Bytes] T_Null},
  "B" = interact {
    acceptParams = T_Fun [T_UInt256,T_UInt256] T_Null,
    endsWith = T_Fun [T_Bytes] T_Null,
    getHand = T_Fun [] T_Bytes,
    partnerIs = T_Fun [T_Address] T_Null,
    random = T_Fun [] T_UInt256,
    shows = T_Fun [] T_Null},
  "O" = interact {
    }};

only("A") {
  const v2 = interact("A")."getParams"();
  const v3 = v2[0];
  const v4 = v2[1];
   };
only("A") {
  const v8 = ADD(v3,v4);
   };
publish("A", join(v7))(v3,v4)(v5, v6).pay(v8){
  const v9 = ADD(v5,v6);
  const v10 = TXN_VALUE();
  const v11 = PEQ(v9,v10);
  claim(CT_Require)(v11);
  commit();
  only("B") {
    const v13 = interact("B")."partnerIs"(v7);
    const v14 = interact("B")."acceptParams"(v5,v6);
     };
  only("B") {
     };
  publish("B", join(v15))()().pay(v5).timeout((DLC_Int 10, {
    only("A") {
       };
    publish("A", again(v7))()().pay(DLC_Int 0){
      const v20 = TXN_VALUE();
      const v21 = PEQ(DLC_Int 0,v20);
      claim(CT_Require)(v21);
      const v22 = BALANCE();
      transfer.(v22).to(v7);
      commit();
      only("A") {
        claim(CT_Require)(DLC_Bool True);
        const v28 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
         };
      only("B") {
        claim(CT_Require)(DLC_Bool True);
        const v33 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
         };
      exit(); } })){
    const v16 = TXN_VALUE();
    const v17 = PEQ(v5,v16);
    claim(CT_Require)(v17);
    commit();
    only("A") {
      const v35 = interact("A")."partnerIs"(v15);
       };
    only("A") {
       };
    publish("A", again(v7))()().pay(DLC_Int 0).timeout((DLC_Int 10, {
      only("B") {
         };
      publish("B", again(v15))()().pay(DLC_Int 0){
        const v40 = TXN_VALUE();
        const v41 = PEQ(DLC_Int 0,v40);
        claim(CT_Require)(v41);
        const v42 = BALANCE();
        transfer.(v42).to(v15);
        commit();
        only("A") {
          claim(CT_Require)(DLC_Bool True);
          const v48 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
           };
        only("B") {
          claim(CT_Require)(DLC_Bool True);
          const v53 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
           };
        exit(); } })){
      const v36 = TXN_VALUE();
      const v37 = PEQ(DLC_Int 0,v36);
      claim(CT_Require)(v37);
      loopvar {
        v54 = DLC_Int 0,
        v55 = DLC_Int 1};
      invariant{
        const v56 = BALANCE();
        const v57 = MUL(DLC_Int 2,v5);
        const v58 = ADD(v57,v6);
        const v59 = PEQ(v56,v58);
        const v61 = PLE(DLC_Int 0,v55);
        const v62 = PLT(v55,DLC_Int 5);
        const v64 = IF_THEN_ELSE(v61,v62,DLC_Bool False);
        const v66 = IF_THEN_ELSE(v59,v64,DLC_Bool False);
        const v68 = PEQ(v55,DLC_Int 3);
        const v70 = IF_THEN_ELSE(v68,DLC_Bool False,DLC_Bool True);
        const v72 = IF_THEN_ELSE(v66,v70,DLC_Bool False);
        const v74 = PEQ(v55,DLC_Int 4);
        const v76 = IF_THEN_ELSE(v74,DLC_Bool False,DLC_Bool True);
        const v78 = IF_THEN_ELSE(v72,v76,DLC_Bool False);
        
        return v78; }
      while{
        const v79 = PEQ(v55,DLC_Int 1);
        
        return v79; }
      {
        commit();
        only("A") {
          let v83;
          const v84 = interact("A")."getHand"();
          const v85 = BYTES_EQ(v84,DLC_Bytes "ROCK");
          const v86 = BYTES_EQ(v84,DLC_Bytes "PAPER");
          const v87 = BYTES_EQ(v84,DLC_Bytes "SCISSORS");
          const v89 = IF_THEN_ELSE(v85,DLC_Bool True,v86);
          const v91 = IF_THEN_ELSE(v89,DLC_Bool True,v87);
          claim(CT_Assume)(v91);
          if v85 then {
            v83 = DLC_Int 0;
             }
          else {
            if v86 then {
              v83 = DLC_Int 1;
               }
            else {
              v83 = DLC_Int 2;
               };
             };
          const v94 = PLE(DLC_Int 0,v83);
          const v95 = PLT(v83,DLC_Int 3);
          const v97 = IF_THEN_ELSE(v94,v95,DLC_Bool False);
          claim(CT_Assert)(v97);
          const v99 = interact("A")."random"();
          const v100 = digest(v99,v83);
          const v101 = interact("A")."commits"();
           };
        only("A") {
           };
        publish("A", again(v7))(v100)(v102).pay(DLC_Int 0).timeout((DLC_Int 10, {
          only("B") {
             };
          publish("B", again(v15))()().pay(DLC_Int 0){
            const v107 = TXN_VALUE();
            const v108 = PEQ(DLC_Int 0,v107);
            claim(CT_Require)(v108);
            const v109 = BALANCE();
            transfer.(v109).to(v15);
            commit();
            only("A") {
              claim(CT_Require)(DLC_Bool True);
              const v115 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
               };
            only("B") {
              claim(CT_Require)(DLC_Bool True);
              const v120 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
               };
            exit(); } })){
          const v103 = TXN_VALUE();
          const v104 = PEQ(DLC_Int 0,v103);
          claim(CT_Require)(v104);
          commit();
          only("B") {
            let v123;
            const v124 = interact("B")."getHand"();
            const v125 = BYTES_EQ(v124,DLC_Bytes "ROCK");
            const v126 = BYTES_EQ(v124,DLC_Bytes "PAPER");
            const v127 = BYTES_EQ(v124,DLC_Bytes "SCISSORS");
            const v129 = IF_THEN_ELSE(v125,DLC_Bool True,v126);
            const v131 = IF_THEN_ELSE(v129,DLC_Bool True,v127);
            claim(CT_Assume)(v131);
            if v125 then {
              v123 = DLC_Int 0;
               }
            else {
              if v126 then {
                v123 = DLC_Int 1;
                 }
              else {
                v123 = DLC_Int 2;
                 };
               };
            const v134 = PLE(DLC_Int 0,v123);
            const v135 = PLT(v123,DLC_Int 3);
            const v137 = IF_THEN_ELSE(v134,v135,DLC_Bool False);
            claim(CT_Assert)(v137);
            const v138 = interact("B")."shows"();
             };
          only("B") {
             };
          publish("B", again(v15))(v123)(v139).pay(DLC_Int 0).timeout((DLC_Int 10, {
            only("A") {
               };
            publish("A", again(v7))()().pay(DLC_Int 0){
              const v144 = TXN_VALUE();
              const v145 = PEQ(DLC_Int 0,v144);
              claim(CT_Require)(v145);
              const v146 = BALANCE();
              transfer.(v146).to(v7);
              commit();
              only("A") {
                claim(CT_Require)(DLC_Bool True);
                const v152 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
                 };
              only("B") {
                claim(CT_Require)(DLC_Bool True);
                const v157 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
                 };
              exit(); } })){
            const v140 = TXN_VALUE();
            const v141 = PEQ(DLC_Int 0,v140);
            claim(CT_Require)(v141);
            const v159 = PLE(DLC_Int 0,v139);
            const v160 = PLT(v139,DLC_Int 3);
            const v162 = IF_THEN_ELSE(v159,v160,DLC_Bool False);
            claim(CT_Require)(v162);
            commit();
            only("A") {
              let v164;
              const v166 = PLE(DLC_Int 0,v139);
              const v167 = PLT(v139,DLC_Int 3);
              const v169 = IF_THEN_ELSE(v166,v167,DLC_Bool False);
              claim(CT_Require)(v169);
              const v170 = PEQ(v139,DLC_Int 0);
              if v170 then {
                v164 = DLC_Bytes "ROCK";
                 }
              else {
                const v171 = PEQ(v139,DLC_Int 1);
                if v171 then {
                  v164 = DLC_Bytes "PAPER";
                   }
                else {
                  v164 = DLC_Bytes "SCISSORS";
                   };
                 };
              const v172 = interact("A")."reveals"(v164);
               };
            only("A") {
               };
            publish("A", again(v7))(v99,v83)(v173, v174).pay(DLC_Int 0).timeout((DLC_Int 10, {
              only("B") {
                 };
              publish("B", again(v15))()().pay(DLC_Int 0){
                const v179 = TXN_VALUE();
                const v180 = PEQ(DLC_Int 0,v179);
                claim(CT_Require)(v180);
                const v181 = BALANCE();
                transfer.(v181).to(v15);
                commit();
                only("A") {
                  claim(CT_Require)(DLC_Bool True);
                  const v187 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                   };
                only("B") {
                  claim(CT_Require)(DLC_Bool True);
                  const v192 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                   };
                exit(); } })){
              const v175 = TXN_VALUE();
              const v176 = PEQ(DLC_Int 0,v175);
              claim(CT_Require)(v176);
              const v194 = digest(v173,v174);
              const v195 = PEQ(v102,v194);
              claim(CT_Require)(v195);
              const v197 = PLE(DLC_Int 0,v174);
              const v198 = PLT(v174,DLC_Int 3);
              const v200 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
              claim(CT_Require)(v200);
              let v202;
              const v204 = PLE(DLC_Int 0,v174);
              const v205 = PLT(v174,DLC_Int 3);
              const v207 = IF_THEN_ELSE(v204,v205,DLC_Bool False);
              const v209 = PLE(DLC_Int 0,v139);
              const v210 = PLT(v139,DLC_Int 3);
              const v212 = IF_THEN_ELSE(v209,v210,DLC_Bool False);
              const v214 = IF_THEN_ELSE(v207,v212,DLC_Bool False);
              if v214 then {
                const v215 = SUB(DLC_Int 4,v139);
                const v216 = ADD(v174,v215);
                const v217 = MOD(v216,DLC_Int 3);
                v202 = v217;
                 }
              else {
                if v207 then {
                  v202 = DLC_Int 2;
                   }
                else {
                  if v212 then {
                    v202 = DLC_Int 0;
                     }
                  else {
                    v202 = DLC_Int 1;
                     };
                   };
                 };
              const v220 = PLE(DLC_Int 0,v202);
              const v221 = PLT(v202,DLC_Int 5);
              const v223 = IF_THEN_ELSE(v220,v221,DLC_Bool False);
              claim(CT_Assert)(v223);
              const v224 = PEQ(v202,DLC_Int 2);
              const v226 = PLE(DLC_Int 0,v174);
              const v227 = PLT(v174,DLC_Int 3);
              const v229 = IF_THEN_ELSE(v226,v227,DLC_Bool False);
              const v232 = IF_THEN_ELSE(v224,DLC_Bool False,DLC_Bool True);
              const v234 = IF_THEN_ELSE(v232,DLC_Bool True,v229);
              claim(CT_Assert)(v234);
              const v235 = PEQ(v202,DLC_Int 0);
              const v237 = PLE(DLC_Int 0,v139);
              const v238 = PLT(v139,DLC_Int 3);
              const v240 = IF_THEN_ELSE(v237,v238,DLC_Bool False);
              const v243 = IF_THEN_ELSE(v235,DLC_Bool False,DLC_Bool True);
              const v245 = IF_THEN_ELSE(v243,DLC_Bool True,v240);
              claim(CT_Assert)(v245);
              const v247 = PEQ(v202,DLC_Int 2);
              const v250 = PEQ(v174,DLC_Int 0);
              const v252 = IF_THEN_ELSE(v250,v247,DLC_Bool False);
              claim(CT_Possible)(v252);
              const v254 = PEQ(v174,DLC_Int 1);
              const v256 = IF_THEN_ELSE(v254,v247,DLC_Bool False);
              claim(CT_Possible)(v256);
              const v258 = PEQ(v174,DLC_Int 2);
              const v260 = IF_THEN_ELSE(v258,v247,DLC_Bool False);
              claim(CT_Possible)(v260);
              const v261 = PEQ(v202,DLC_Int 0);
              const v264 = PEQ(v139,DLC_Int 0);
              const v266 = IF_THEN_ELSE(v264,v261,DLC_Bool False);
              claim(CT_Possible)(v266);
              const v268 = PEQ(v139,DLC_Int 1);
              const v270 = IF_THEN_ELSE(v268,v261,DLC_Bool False);
              claim(CT_Possible)(v270);
              const v272 = PEQ(v139,DLC_Int 2);
              const v274 = IF_THEN_ELSE(v272,v261,DLC_Bool False);
              claim(CT_Possible)(v274);
              commit();
              only("B") {
                 };
              publish("B", again(v15))()().pay(DLC_Int 0).timeout((DLC_Int 10, {
                only("A") {
                   };
                publish("A", again(v7))()().pay(DLC_Int 0){
                  const v279 = TXN_VALUE();
                  const v280 = PEQ(DLC_Int 0,v279);
                  claim(CT_Require)(v280);
                  const v281 = BALANCE();
                  transfer.(v281).to(v7);
                  commit();
                  only("A") {
                    claim(CT_Require)(DLC_Bool True);
                    const v287 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
                     };
                  only("B") {
                    claim(CT_Require)(DLC_Bool True);
                    const v292 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
                     };
                  exit(); } })){
                const v275 = TXN_VALUE();
                const v276 = PEQ(DLC_Int 0,v275);
                claim(CT_Require)(v276);
                const v293 = ADD(DLC_Int 1,v54);
                {
                  v54 = v293,
                  v55 = v202}
                continue; } } } } }
      const v295 = PEQ(v55,DLC_Int 1);
      const v297 = IF_THEN_ELSE(v295,DLC_Bool False,DLC_Bool True);
      claim(CT_Assert)(v297);
      let v299;
      const v300 = PEQ(v55,DLC_Int 2);
      if v300 then {
        const v301 = MUL(DLC_Int 2,v5);
        v299 = [v301,DLC_Int 0];
         }
      else {
        const v302 = PEQ(v55,DLC_Int 0);
        if v302 then {
          const v303 = MUL(DLC_Int 2,v5);
          v299 = [DLC_Int 0,v303];
           }
        else {
          v299 = [v5,v5];
           };
         };
      const v304 = v299[0];
      const v305 = v299[1];
      const v306 = ADD(v6,v304);
      transfer.(v306).to(v7);
      transfer.(v305).to(v15);
      commit();
      only("A") {
        let v310;
        const v312 = PLE(DLC_Int 0,v55);
        const v313 = PLT(v55,DLC_Int 5);
        const v315 = IF_THEN_ELSE(v312,v313,DLC_Bool False);
        claim(CT_Require)(v315);
        const v316 = PEQ(v55,DLC_Int 0);
        if v316 then {
          v310 = DLC_Bytes "Bob wins";
           }
        else {
          const v317 = PEQ(v55,DLC_Int 1);
          if v317 then {
            v310 = DLC_Bytes "Draw";
             }
          else {
            const v318 = PEQ(v55,DLC_Int 2);
            if v318 then {
              v310 = DLC_Bytes "Alice wins";
               }
            else {
              const v319 = PEQ(v55,DLC_Int 3);
              if v319 then {
                v310 = DLC_Bytes "Alice quits";
                 }
              else {
                v310 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v320 = interact("A")."endsWith"(v310);
         };
      only("B") {
        let v322;
        const v324 = PLE(DLC_Int 0,v55);
        const v325 = PLT(v55,DLC_Int 5);
        const v327 = IF_THEN_ELSE(v324,v325,DLC_Bool False);
        claim(CT_Require)(v327);
        const v328 = PEQ(v55,DLC_Int 0);
        if v328 then {
          v322 = DLC_Bytes "Bob wins";
           }
        else {
          const v329 = PEQ(v55,DLC_Int 1);
          if v329 then {
            v322 = DLC_Bytes "Draw";
             }
          else {
            const v330 = PEQ(v55,DLC_Int 2);
            if v330 then {
              v322 = DLC_Bytes "Alice wins";
               }
            else {
              const v331 = PEQ(v55,DLC_Int 3);
              if v331 then {
                v322 = DLC_Bytes "Alice quits";
                 }
              else {
                v322 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v332 = interact("B")."endsWith"(v322);
         };
      exit(); } } }