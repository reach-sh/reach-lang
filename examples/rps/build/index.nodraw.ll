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
          claim(CT_Assert)(DLC_Bool True);
          const v34 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
           };
        only("B") {
          claim(CT_Assert)(DLC_Bool True);
          const v42 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
           };
        exit(); } })){
    const v17 = TXN_VALUE();
    const v19 = PEQ(v5,v17);
    claim(CT_Require)(v19);
    commit();
    only("A") {
      const v44 = interact("A")."partnerIs"(v16);
       };
    only("A") {
       };
    publish("A", again(v7))()().pay(DLC_Int 0)
      .timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again(v16))()().pay(DLC_Int 0){
          const v50 = TXN_VALUE();
          const v52 = PEQ(DLC_Int 0,v50);
          claim(CT_Require)(v52);
          const v53 = BALANCE();
          transfer.(v53).to(v16);
          commit();
          only("A") {
            claim(CT_Assert)(DLC_Bool True);
            const v62 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
             };
          only("B") {
            claim(CT_Assert)(DLC_Bool True);
            const v70 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
             };
          exit(); } })){
      const v45 = TXN_VALUE();
      const v47 = PEQ(DLC_Int 0,v45);
      claim(CT_Require)(v47);
      loopvar {
        v71 = DLC_Int 0,
        v72 = DLC_Int 1};
      invariant{
        const v73 = BALANCE();
        const v74 = MUL(DLC_Int 2,v5);
        const v75 = ADD(v74,v6);
        const v77 = PEQ(v73,v75);
        const v79 = PLE(DLC_Int 0,v72);
        const v80 = PLT(v72,DLC_Int 5);
        const v81 = IF_THEN_ELSE(v79,v80,DLC_Bool False);
        const v82 = IF_THEN_ELSE(v77,v81,DLC_Bool False);
        const v85 = PEQ(v72,DLC_Int 3);
        const v87 = IF_THEN_ELSE(v85,DLC_Bool False,DLC_Bool True);
        const v88 = IF_THEN_ELSE(v82,v87,DLC_Bool False);
        const v91 = PEQ(v72,DLC_Int 4);
        const v93 = IF_THEN_ELSE(v91,DLC_Bool False,DLC_Bool True);
        const v94 = IF_THEN_ELSE(v88,v93,DLC_Bool False);
        
        return v94; }
      while{
        const v96 = PEQ(v72,DLC_Int 1);
        
        return v96; }
      {
        commit();
        only("A") {
          let v100;
          const v101 = interact("A")."getHand"();
          const v103 = BYTES_EQ(v101,DLC_Bytes "ROCK");
          const v105 = BYTES_EQ(v101,DLC_Bytes "PAPER");
          const v107 = BYTES_EQ(v101,DLC_Bytes "SCISSORS");
          const v108 = IF_THEN_ELSE(v103,DLC_Bool True,v105);
          const v109 = IF_THEN_ELSE(v108,DLC_Bool True,v107);
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
          const v112 = PLE(DLC_Int 0,v100);
          const v113 = PLT(v100,DLC_Int 3);
          const v114 = IF_THEN_ELSE(v112,v113,DLC_Bool False);
          claim(CT_Assert)(v114);
          const v116 = interact("A")."random"();
          const v117 = digest(v116,v100);
          const v118 = interact("A")."commits"();
           };
        only("A") {
           };
        publish("A", again(v7))(v117)(v119).pay(DLC_Int 0)
          .timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again(v16))()().pay(DLC_Int 0){
              const v125 = TXN_VALUE();
              const v127 = PEQ(DLC_Int 0,v125);
              claim(CT_Require)(v127);
              const v128 = BALANCE();
              transfer.(v128).to(v16);
              commit();
              only("A") {
                claim(CT_Assert)(DLC_Bool True);
                const v137 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                 };
              only("B") {
                claim(CT_Assert)(DLC_Bool True);
                const v145 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                 };
              exit(); } })){
          const v120 = TXN_VALUE();
          const v122 = PEQ(DLC_Int 0,v120);
          claim(CT_Require)(v122);
          commit();
          only("B") {
            let v148;
            const v149 = interact("B")."getHand"();
            const v151 = BYTES_EQ(v149,DLC_Bytes "ROCK");
            const v153 = BYTES_EQ(v149,DLC_Bytes "PAPER");
            const v155 = BYTES_EQ(v149,DLC_Bytes "SCISSORS");
            const v156 = IF_THEN_ELSE(v151,DLC_Bool True,v153);
            const v157 = IF_THEN_ELSE(v156,DLC_Bool True,v155);
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
            const v160 = PLE(DLC_Int 0,v148);
            const v161 = PLT(v148,DLC_Int 3);
            const v162 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
            claim(CT_Assert)(v162);
            const v163 = interact("B")."shows"();
             };
          only("B") {
             };
          publish("B", again(v16))(v148)(v164).pay(DLC_Int 0)
            .timeout((DLC_Int 10, {
              only("A") {
                 };
              publish("A", again(v7))()().pay(DLC_Int 0){
                const v170 = TXN_VALUE();
                const v172 = PEQ(DLC_Int 0,v170);
                claim(CT_Require)(v172);
                const v173 = BALANCE();
                transfer.(v173).to(v7);
                commit();
                only("A") {
                  claim(CT_Assert)(DLC_Bool True);
                  const v182 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
                   };
                only("B") {
                  claim(CT_Assert)(DLC_Bool True);
                  const v190 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
                   };
                exit(); } })){
            const v165 = TXN_VALUE();
            const v167 = PEQ(DLC_Int 0,v165);
            claim(CT_Require)(v167);
            const v192 = PLE(DLC_Int 0,v164);
            const v193 = PLT(v164,DLC_Int 3);
            const v194 = IF_THEN_ELSE(v192,v193,DLC_Bool False);
            claim(CT_Require)(v194);
            commit();
            only("A") {
              let v196;
              const v198 = PLE(DLC_Int 0,v164);
              const v199 = PLT(v164,DLC_Int 3);
              const v200 = IF_THEN_ELSE(v198,v199,DLC_Bool False);
              claim(CT_Assert)(v200);
              const v202 = PEQ(v164,DLC_Int 0);
              if v202 then {
                v196 = DLC_Bytes "ROCK";
                 }
              else {
                const v204 = PEQ(v164,DLC_Int 1);
                if v204 then {
                  v196 = DLC_Bytes "PAPER";
                   }
                else {
                  v196 = DLC_Bytes "SCISSORS";
                   };
                 };
              const v205 = interact("A")."reveals"(v196);
               };
            only("A") {
               };
            publish("A", again(v7))(v116,v100)(v206, v207).pay(DLC_Int 0)
              .timeout((DLC_Int 10, {
                only("B") {
                   };
                publish("B", again(v16))()().pay(DLC_Int 0){
                  const v213 = TXN_VALUE();
                  const v215 = PEQ(DLC_Int 0,v213);
                  claim(CT_Require)(v215);
                  const v216 = BALANCE();
                  transfer.(v216).to(v16);
                  commit();
                  only("A") {
                    claim(CT_Assert)(DLC_Bool True);
                    const v225 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                     };
                  only("B") {
                    claim(CT_Assert)(DLC_Bool True);
                    const v233 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                     };
                  exit(); } })){
              const v208 = TXN_VALUE();
              const v210 = PEQ(DLC_Int 0,v208);
              claim(CT_Require)(v210);
              const v235 = digest(v206,v207);
              const v237 = PEQ(v119,v235);
              claim(CT_Require)(v237);
              const v239 = PLE(DLC_Int 0,v207);
              const v240 = PLT(v207,DLC_Int 3);
              const v241 = IF_THEN_ELSE(v239,v240,DLC_Bool False);
              claim(CT_Require)(v241);
              let v243;
              const v245 = PLE(DLC_Int 0,v207);
              const v246 = PLT(v207,DLC_Int 3);
              const v247 = IF_THEN_ELSE(v245,v246,DLC_Bool False);
              const v249 = PLE(DLC_Int 0,v164);
              const v250 = PLT(v164,DLC_Int 3);
              const v251 = IF_THEN_ELSE(v249,v250,DLC_Bool False);
              const v252 = IF_THEN_ELSE(v247,v251,DLC_Bool False);
              if v252 then {
                const v253 = SUB(DLC_Int 4,v164);
                const v254 = ADD(v207,v253);
                const v255 = MOD(v254,DLC_Int 3);
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
              const v258 = PLE(DLC_Int 0,v243);
              const v259 = PLT(v243,DLC_Int 5);
              const v260 = IF_THEN_ELSE(v258,v259,DLC_Bool False);
              claim(CT_Assert)(v260);
              const v262 = PEQ(v243,DLC_Int 2);
              const v264 = PLE(DLC_Int 0,v207);
              const v265 = PLT(v207,DLC_Int 3);
              const v266 = IF_THEN_ELSE(v264,v265,DLC_Bool False);
              const v269 = IF_THEN_ELSE(v262,DLC_Bool False,DLC_Bool True);
              const v270 = IF_THEN_ELSE(v269,DLC_Bool True,v266);
              claim(CT_Assert)(v270);
              const v272 = PEQ(v243,DLC_Int 0);
              const v274 = PLE(DLC_Int 0,v164);
              const v275 = PLT(v164,DLC_Int 3);
              const v276 = IF_THEN_ELSE(v274,v275,DLC_Bool False);
              const v279 = IF_THEN_ELSE(v272,DLC_Bool False,DLC_Bool True);
              const v280 = IF_THEN_ELSE(v279,DLC_Bool True,v276);
              claim(CT_Assert)(v280);
              const v283 = PEQ(v243,DLC_Int 2);
              const v287 = PEQ(v207,DLC_Int 0);
              const v288 = IF_THEN_ELSE(v287,v283,DLC_Bool False);
              claim(CT_Possible)(v288);
              const v291 = PEQ(v207,DLC_Int 1);
              const v292 = IF_THEN_ELSE(v291,v283,DLC_Bool False);
              claim(CT_Possible)(v292);
              const v295 = PEQ(v207,DLC_Int 2);
              const v296 = IF_THEN_ELSE(v295,v283,DLC_Bool False);
              claim(CT_Possible)(v296);
              const v298 = PEQ(v243,DLC_Int 0);
              const v302 = PEQ(v164,DLC_Int 0);
              const v303 = IF_THEN_ELSE(v302,v298,DLC_Bool False);
              claim(CT_Possible)(v303);
              const v306 = PEQ(v164,DLC_Int 1);
              const v307 = IF_THEN_ELSE(v306,v298,DLC_Bool False);
              claim(CT_Possible)(v307);
              const v310 = PEQ(v164,DLC_Int 2);
              const v311 = IF_THEN_ELSE(v310,v298,DLC_Bool False);
              claim(CT_Possible)(v311);
              const v312 = ADD(DLC_Int 1,v71);
              {
                v71 = v312,
                v72 = v243}
              continue; } } } }
      const v315 = PEQ(v72,DLC_Int 1);
      const v317 = IF_THEN_ELSE(v315,DLC_Bool False,DLC_Bool True);
      claim(CT_Assert)(v317);
      let v319;
      const v321 = PEQ(v72,DLC_Int 2);
      if v321 then {
        const v322 = MUL(DLC_Int 2,v5);
        v319 = [v322,DLC_Int 0];
         }
      else {
        const v324 = PEQ(v72,DLC_Int 0);
        if v324 then {
          const v325 = MUL(DLC_Int 2,v5);
          v319 = [DLC_Int 0,v325];
           }
        else {
          v319 = [v5,v5];
           };
         };
      const v326 = v319[0];
      const v327 = v319[1];
      const v328 = ADD(v6,v326);
      transfer.(v328).to(v7);
      transfer.(v327).to(v16);
      commit();
      only("A") {
        let v332;
        const v334 = PLE(DLC_Int 0,v72);
        const v335 = PLT(v72,DLC_Int 5);
        const v336 = IF_THEN_ELSE(v334,v335,DLC_Bool False);
        claim(CT_Assert)(v336);
        const v338 = PEQ(v72,DLC_Int 0);
        if v338 then {
          v332 = DLC_Bytes "Bob wins";
           }
        else {
          const v340 = PEQ(v72,DLC_Int 1);
          if v340 then {
            v332 = DLC_Bytes "Draw";
             }
          else {
            const v342 = PEQ(v72,DLC_Int 2);
            if v342 then {
              v332 = DLC_Bytes "Alice wins";
               }
            else {
              const v344 = PEQ(v72,DLC_Int 3);
              if v344 then {
                v332 = DLC_Bytes "Alice quits";
                 }
              else {
                v332 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v345 = interact("A")."endsWith"(v332);
         };
      only("B") {
        let v347;
        const v349 = PLE(DLC_Int 0,v72);
        const v350 = PLT(v72,DLC_Int 5);
        const v351 = IF_THEN_ELSE(v349,v350,DLC_Bool False);
        claim(CT_Assert)(v351);
        const v353 = PEQ(v72,DLC_Int 0);
        if v353 then {
          v347 = DLC_Bytes "Bob wins";
           }
        else {
          const v355 = PEQ(v72,DLC_Int 1);
          if v355 then {
            v347 = DLC_Bytes "Draw";
             }
          else {
            const v357 = PEQ(v72,DLC_Int 2);
            if v357 then {
              v347 = DLC_Bytes "Alice wins";
               }
            else {
              const v359 = PEQ(v72,DLC_Int 3);
              if v359 then {
                v347 = DLC_Bytes "Alice quits";
                 }
              else {
                v347 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v360 = interact("B")."endsWith"(v347);
         };
      exit(); } } }