#lang ll
parts {
  "A" = interact {
    commits = Fun([], Null),
    endsWith = Fun([Bytes], Null),
    getHand = Fun([], Bytes),
    getParams = Fun([], Tuple(UInt256, UInt256)),
    partnerIs = Fun([Address], Null),
    random = Fun([], UInt256),
    reveals = Fun([Bytes], Null)},
  "B" = interact {
    acceptParams = Fun([UInt256, UInt256], Null),
    endsWith = Fun([Bytes], Null),
    getHand = Fun([], Bytes),
    partnerIs = Fun([Address], Null),
    random = Fun([], UInt256),
    shows = Fun([], Null)},
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
  const v12 = PEQ(v9,v10);
  claim(CT_Require)(v12);
  commit();
  only("B") {
    const v14 = interact("B")."partnerIs"(v7);
    const v15 = interact("B")."acceptParams"(v5,v6);
     };
  only("B") {
     };
  publish("B", join(v16))()().pay(v5)
    .timeout((DLC_Int 10, {
      only("A") {
         };
      publish("A", again(v7))()().pay(DLC_Int 0){
        const v22 = TXN_VALUE();
        const v24 = PEQ(DLC_Int 0,v22);
        claim(CT_Require)(v24);
        const v25 = BALANCE();
        transfer.(v25).to(v7);
        commit();
        only("A") {
          claim(CT_Require)(DLC_Bool True);
          const v35 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
           };
        only("B") {
          claim(CT_Require)(DLC_Bool True);
          const v44 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
           };
        exit(); } })){
    const v17 = TXN_VALUE();
    const v19 = PEQ(v5,v17);
    claim(CT_Require)(v19);
    commit();
    only("A") {
      const v46 = interact("A")."partnerIs"(v16);
       };
    only("A") {
      let v50;
      const v51 = interact("A")."getHand"();
      const v53 = BYTES_EQ(v51,DLC_Bytes "ROCK");
      const v55 = BYTES_EQ(v51,DLC_Bytes "PAPER");
      const v57 = BYTES_EQ(v51,DLC_Bytes "SCISSORS");
      const v59 = IF_THEN_ELSE(v53,DLC_Bool True,v55);
      const v61 = IF_THEN_ELSE(v59,DLC_Bool True,v57);
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
      const v64 = PLE(DLC_Int 0,v50);
      const v65 = PLT(v50,DLC_Int 3);
      const v67 = IF_THEN_ELSE(v64,v65,DLC_Bool False);
      claim(CT_Assert)(v67);
      const v69 = interact("A")."random"();
      const v70 = digest(v69,v50);
      const v71 = interact("A")."commits"();
       };
    only("A") {
       };
    publish("A", again(v7))(v70)(v72).pay(DLC_Int 0)
      .timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again(v16))()().pay(DLC_Int 0){
          const v78 = TXN_VALUE();
          const v80 = PEQ(DLC_Int 0,v78);
          claim(CT_Require)(v80);
          const v81 = BALANCE();
          transfer.(v81).to(v16);
          commit();
          only("A") {
            claim(CT_Require)(DLC_Bool True);
            const v91 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
             };
          only("B") {
            claim(CT_Require)(DLC_Bool True);
            const v100 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
             };
          exit(); } })){
      const v73 = TXN_VALUE();
      const v75 = PEQ(DLC_Int 0,v73);
      claim(CT_Require)(v75);
      commit();
      only("B") {
        let v103;
        const v104 = interact("B")."getHand"();
        const v106 = BYTES_EQ(v104,DLC_Bytes "ROCK");
        const v108 = BYTES_EQ(v104,DLC_Bytes "PAPER");
        const v110 = BYTES_EQ(v104,DLC_Bytes "SCISSORS");
        const v112 = IF_THEN_ELSE(v106,DLC_Bool True,v108);
        const v114 = IF_THEN_ELSE(v112,DLC_Bool True,v110);
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
        const v117 = PLE(DLC_Int 0,v103);
        const v118 = PLT(v103,DLC_Int 3);
        const v120 = IF_THEN_ELSE(v117,v118,DLC_Bool False);
        claim(CT_Assert)(v120);
        const v121 = interact("B")."shows"();
         };
      only("B") {
         };
      publish("B", again(v16))(v103)(v122).pay(DLC_Int 0)
        .timeout((DLC_Int 10, {
          only("A") {
             };
          publish("A", again(v7))()().pay(DLC_Int 0){
            const v128 = TXN_VALUE();
            const v130 = PEQ(DLC_Int 0,v128);
            claim(CT_Require)(v130);
            const v131 = BALANCE();
            transfer.(v131).to(v7);
            commit();
            only("A") {
              claim(CT_Require)(DLC_Bool True);
              const v141 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
               };
            only("B") {
              claim(CT_Require)(DLC_Bool True);
              const v150 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
               };
            exit(); } })){
        const v123 = TXN_VALUE();
        const v125 = PEQ(DLC_Int 0,v123);
        claim(CT_Require)(v125);
        const v152 = PLE(DLC_Int 0,v122);
        const v153 = PLT(v122,DLC_Int 3);
        const v155 = IF_THEN_ELSE(v152,v153,DLC_Bool False);
        claim(CT_Require)(v155);
        commit();
        only("A") {
          let v157;
          const v159 = PLE(DLC_Int 0,v122);
          const v160 = PLT(v122,DLC_Int 3);
          const v162 = IF_THEN_ELSE(v159,v160,DLC_Bool False);
          claim(CT_Require)(v162);
          const v164 = PEQ(v122,DLC_Int 0);
          if v164 then {
            v157 = DLC_Bytes "ROCK";
             }
          else {
            const v166 = PEQ(v122,DLC_Int 1);
            if v166 then {
              v157 = DLC_Bytes "PAPER";
               }
            else {
              v157 = DLC_Bytes "SCISSORS";
               };
             };
          const v167 = interact("A")."reveals"(v157);
           };
        only("A") {
           };
        publish("A", again(v7))(v69,v50)(v168, v169).pay(DLC_Int 0)
          .timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again(v16))()().pay(DLC_Int 0){
              const v175 = TXN_VALUE();
              const v177 = PEQ(DLC_Int 0,v175);
              claim(CT_Require)(v177);
              const v178 = BALANCE();
              transfer.(v178).to(v16);
              commit();
              only("A") {
                claim(CT_Require)(DLC_Bool True);
                const v188 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                 };
              only("B") {
                claim(CT_Require)(DLC_Bool True);
                const v197 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                 };
              exit(); } })){
          const v170 = TXN_VALUE();
          const v172 = PEQ(DLC_Int 0,v170);
          claim(CT_Require)(v172);
          const v199 = digest(v168,v169);
          const v201 = PEQ(v72,v199);
          claim(CT_Require)(v201);
          const v203 = PLE(DLC_Int 0,v169);
          const v204 = PLT(v169,DLC_Int 3);
          const v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
          claim(CT_Require)(v206);
          let v208;
          const v210 = PLE(DLC_Int 0,v169);
          const v211 = PLT(v169,DLC_Int 3);
          const v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
          const v215 = PLE(DLC_Int 0,v122);
          const v216 = PLT(v122,DLC_Int 3);
          const v218 = IF_THEN_ELSE(v215,v216,DLC_Bool False);
          const v220 = IF_THEN_ELSE(v213,v218,DLC_Bool False);
          if v220 then {
            const v221 = SUB(DLC_Int 4,v122);
            const v222 = ADD(v169,v221);
            const v223 = MOD(v222,DLC_Int 3);
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
          const v226 = PLE(DLC_Int 0,v208);
          const v227 = PLT(v208,DLC_Int 5);
          const v229 = IF_THEN_ELSE(v226,v227,DLC_Bool False);
          claim(CT_Assert)(v229);
          const v231 = PEQ(v208,DLC_Int 2);
          const v233 = PLE(DLC_Int 0,v169);
          const v234 = PLT(v169,DLC_Int 3);
          const v236 = IF_THEN_ELSE(v233,v234,DLC_Bool False);
          const v239 = IF_THEN_ELSE(v231,DLC_Bool False,DLC_Bool True);
          const v241 = IF_THEN_ELSE(v239,DLC_Bool True,v236);
          claim(CT_Assert)(v241);
          const v243 = PEQ(v208,DLC_Int 0);
          const v245 = PLE(DLC_Int 0,v122);
          const v246 = PLT(v122,DLC_Int 3);
          const v248 = IF_THEN_ELSE(v245,v246,DLC_Bool False);
          const v251 = IF_THEN_ELSE(v243,DLC_Bool False,DLC_Bool True);
          const v253 = IF_THEN_ELSE(v251,DLC_Bool True,v248);
          claim(CT_Assert)(v253);
          const v256 = PEQ(v208,DLC_Int 2);
          const v260 = PEQ(v169,DLC_Int 0);
          const v262 = IF_THEN_ELSE(v260,v256,DLC_Bool False);
          claim(CT_Possible)(v262);
          const v265 = PEQ(v169,DLC_Int 1);
          const v267 = IF_THEN_ELSE(v265,v256,DLC_Bool False);
          claim(CT_Possible)(v267);
          const v270 = PEQ(v169,DLC_Int 2);
          const v272 = IF_THEN_ELSE(v270,v256,DLC_Bool False);
          claim(CT_Possible)(v272);
          const v274 = PEQ(v208,DLC_Int 0);
          const v278 = PEQ(v122,DLC_Int 0);
          const v280 = IF_THEN_ELSE(v278,v274,DLC_Bool False);
          claim(CT_Possible)(v280);
          const v283 = PEQ(v122,DLC_Int 1);
          const v285 = IF_THEN_ELSE(v283,v274,DLC_Bool False);
          claim(CT_Possible)(v285);
          const v288 = PEQ(v122,DLC_Int 2);
          const v290 = IF_THEN_ELSE(v288,v274,DLC_Bool False);
          claim(CT_Possible)(v290);
          let v292;
          const v294 = PEQ(v208,DLC_Int 2);
          if v294 then {
            const v295 = MUL(DLC_Int 2,v5);
            v292 = [v295,DLC_Int 0];
             }
          else {
            const v297 = PEQ(v208,DLC_Int 0);
            if v297 then {
              const v298 = MUL(DLC_Int 2,v5);
              v292 = [DLC_Int 0,v298];
               }
            else {
              v292 = [v5,v5];
               };
             };
          const v299 = v292[0];
          const v300 = v292[1];
          const v301 = ADD(v6,v299);
          transfer.(v301).to(v7);
          transfer.(v300).to(v16);
          commit();
          only("A") {
            let v305;
            const v307 = PLE(DLC_Int 0,v208);
            const v308 = PLT(v208,DLC_Int 5);
            const v310 = IF_THEN_ELSE(v307,v308,DLC_Bool False);
            claim(CT_Require)(v310);
            const v312 = PEQ(v208,DLC_Int 0);
            if v312 then {
              v305 = DLC_Bytes "Bob wins";
               }
            else {
              const v314 = PEQ(v208,DLC_Int 1);
              if v314 then {
                v305 = DLC_Bytes "Draw";
                 }
              else {
                const v316 = PEQ(v208,DLC_Int 2);
                if v316 then {
                  v305 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v318 = PEQ(v208,DLC_Int 3);
                  if v318 then {
                    v305 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v305 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v319 = interact("A")."endsWith"(v305);
             };
          only("B") {
            let v321;
            const v323 = PLE(DLC_Int 0,v208);
            const v324 = PLT(v208,DLC_Int 5);
            const v326 = IF_THEN_ELSE(v323,v324,DLC_Bool False);
            claim(CT_Require)(v326);
            const v328 = PEQ(v208,DLC_Int 0);
            if v328 then {
              v321 = DLC_Bytes "Bob wins";
               }
            else {
              const v330 = PEQ(v208,DLC_Int 1);
              if v330 then {
                v321 = DLC_Bytes "Draw";
                 }
              else {
                const v332 = PEQ(v208,DLC_Int 2);
                if v332 then {
                  v321 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v334 = PEQ(v208,DLC_Int 3);
                  if v334 then {
                    v321 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v321 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v335 = interact("B")."endsWith"(v321);
             };
          exit(); } } } } }