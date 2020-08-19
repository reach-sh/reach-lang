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
       };
    publish("A", again(v7))()().pay(DLC_Int 0)
      .timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again(v16))()().pay(DLC_Int 0){
          const v52 = TXN_VALUE();
          const v54 = PEQ(DLC_Int 0,v52);
          claim(CT_Require)(v54);
          const v55 = BALANCE();
          transfer.(v55).to(v16);
          commit();
          only("A") {
            claim(CT_Require)(DLC_Bool True);
            const v65 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
             };
          only("B") {
            claim(CT_Require)(DLC_Bool True);
            const v74 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
             };
          exit(); } })){
      const v47 = TXN_VALUE();
      const v49 = PEQ(DLC_Int 0,v47);
      claim(CT_Require)(v49);
      loopvar {
        v75 = DLC_Int 0,
        v76 = DLC_Int 1};
      invariant{
        const v77 = BALANCE();
        const v78 = MUL(DLC_Int 2,v5);
        const v79 = ADD(v78,v6);
        const v81 = PEQ(v77,v79);
        const v83 = PLE(DLC_Int 0,v76);
        const v84 = PLT(v76,DLC_Int 5);
        const v86 = IF_THEN_ELSE(v83,v84,DLC_Bool False);
        const v88 = IF_THEN_ELSE(v81,v86,DLC_Bool False);
        const v91 = PEQ(v76,DLC_Int 3);
        const v93 = IF_THEN_ELSE(v91,DLC_Bool False,DLC_Bool True);
        const v95 = IF_THEN_ELSE(v88,v93,DLC_Bool False);
        const v98 = PEQ(v76,DLC_Int 4);
        const v100 = IF_THEN_ELSE(v98,DLC_Bool False,DLC_Bool True);
        const v102 = IF_THEN_ELSE(v95,v100,DLC_Bool False);
        
        return v102; }
      while{
        const v104 = PEQ(v76,DLC_Int 1);
        
        return v104; }
      {
        commit();
        only("A") {
          let v108;
          const v109 = interact("A")."getHand"();
          const v111 = BYTES_EQ(v109,DLC_Bytes "ROCK");
          const v113 = BYTES_EQ(v109,DLC_Bytes "PAPER");
          const v115 = BYTES_EQ(v109,DLC_Bytes "SCISSORS");
          const v117 = IF_THEN_ELSE(v111,DLC_Bool True,v113);
          const v119 = IF_THEN_ELSE(v117,DLC_Bool True,v115);
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
          const v122 = PLE(DLC_Int 0,v108);
          const v123 = PLT(v108,DLC_Int 3);
          const v125 = IF_THEN_ELSE(v122,v123,DLC_Bool False);
          claim(CT_Assert)(v125);
          const v127 = interact("A")."random"();
          const v128 = digest(v127,v108);
          const v129 = interact("A")."commits"();
           };
        only("A") {
           };
        publish("A", again(v7))(v128)(v130).pay(DLC_Int 0)
          .timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again(v16))()().pay(DLC_Int 0){
              const v136 = TXN_VALUE();
              const v138 = PEQ(DLC_Int 0,v136);
              claim(CT_Require)(v138);
              const v139 = BALANCE();
              transfer.(v139).to(v16);
              commit();
              only("A") {
                claim(CT_Require)(DLC_Bool True);
                const v149 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                 };
              only("B") {
                claim(CT_Require)(DLC_Bool True);
                const v158 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                 };
              exit(); } })){
          const v131 = TXN_VALUE();
          const v133 = PEQ(DLC_Int 0,v131);
          claim(CT_Require)(v133);
          commit();
          only("B") {
            let v161;
            const v162 = interact("B")."getHand"();
            const v164 = BYTES_EQ(v162,DLC_Bytes "ROCK");
            const v166 = BYTES_EQ(v162,DLC_Bytes "PAPER");
            const v168 = BYTES_EQ(v162,DLC_Bytes "SCISSORS");
            const v170 = IF_THEN_ELSE(v164,DLC_Bool True,v166);
            const v172 = IF_THEN_ELSE(v170,DLC_Bool True,v168);
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
            const v175 = PLE(DLC_Int 0,v161);
            const v176 = PLT(v161,DLC_Int 3);
            const v178 = IF_THEN_ELSE(v175,v176,DLC_Bool False);
            claim(CT_Assert)(v178);
            const v179 = interact("B")."shows"();
             };
          only("B") {
             };
          publish("B", again(v16))(v161)(v180).pay(DLC_Int 0)
            .timeout((DLC_Int 10, {
              only("A") {
                 };
              publish("A", again(v7))()().pay(DLC_Int 0){
                const v186 = TXN_VALUE();
                const v188 = PEQ(DLC_Int 0,v186);
                claim(CT_Require)(v188);
                const v189 = BALANCE();
                transfer.(v189).to(v7);
                commit();
                only("A") {
                  claim(CT_Require)(DLC_Bool True);
                  const v199 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
                   };
                only("B") {
                  claim(CT_Require)(DLC_Bool True);
                  const v208 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
                   };
                exit(); } })){
            const v181 = TXN_VALUE();
            const v183 = PEQ(DLC_Int 0,v181);
            claim(CT_Require)(v183);
            const v210 = PLE(DLC_Int 0,v180);
            const v211 = PLT(v180,DLC_Int 3);
            const v213 = IF_THEN_ELSE(v210,v211,DLC_Bool False);
            claim(CT_Require)(v213);
            commit();
            only("A") {
              let v215;
              const v217 = PLE(DLC_Int 0,v180);
              const v218 = PLT(v180,DLC_Int 3);
              const v220 = IF_THEN_ELSE(v217,v218,DLC_Bool False);
              claim(CT_Require)(v220);
              const v222 = PEQ(v180,DLC_Int 0);
              if v222 then {
                v215 = DLC_Bytes "ROCK";
                 }
              else {
                const v224 = PEQ(v180,DLC_Int 1);
                if v224 then {
                  v215 = DLC_Bytes "PAPER";
                   }
                else {
                  v215 = DLC_Bytes "SCISSORS";
                   };
                 };
              const v225 = interact("A")."reveals"(v215);
               };
            only("A") {
               };
            publish("A", again(v7))(v127,v108)(v226, v227).pay(DLC_Int 0)
              .timeout((DLC_Int 10, {
                only("B") {
                   };
                publish("B", again(v16))()().pay(DLC_Int 0){
                  const v233 = TXN_VALUE();
                  const v235 = PEQ(DLC_Int 0,v233);
                  claim(CT_Require)(v235);
                  const v236 = BALANCE();
                  transfer.(v236).to(v16);
                  commit();
                  only("A") {
                    claim(CT_Require)(DLC_Bool True);
                    const v246 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                     };
                  only("B") {
                    claim(CT_Require)(DLC_Bool True);
                    const v255 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                     };
                  exit(); } })){
              const v228 = TXN_VALUE();
              const v230 = PEQ(DLC_Int 0,v228);
              claim(CT_Require)(v230);
              const v257 = digest(v226,v227);
              const v259 = PEQ(v130,v257);
              claim(CT_Require)(v259);
              const v261 = PLE(DLC_Int 0,v227);
              const v262 = PLT(v227,DLC_Int 3);
              const v264 = IF_THEN_ELSE(v261,v262,DLC_Bool False);
              claim(CT_Require)(v264);
              let v266;
              const v268 = PLE(DLC_Int 0,v227);
              const v269 = PLT(v227,DLC_Int 3);
              const v271 = IF_THEN_ELSE(v268,v269,DLC_Bool False);
              const v273 = PLE(DLC_Int 0,v180);
              const v274 = PLT(v180,DLC_Int 3);
              const v276 = IF_THEN_ELSE(v273,v274,DLC_Bool False);
              const v278 = IF_THEN_ELSE(v271,v276,DLC_Bool False);
              if v278 then {
                const v279 = SUB(DLC_Int 4,v180);
                const v280 = ADD(v227,v279);
                const v281 = MOD(v280,DLC_Int 3);
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
              const v284 = PLE(DLC_Int 0,v266);
              const v285 = PLT(v266,DLC_Int 5);
              const v287 = IF_THEN_ELSE(v284,v285,DLC_Bool False);
              claim(CT_Assert)(v287);
              const v289 = PEQ(v266,DLC_Int 2);
              const v291 = PLE(DLC_Int 0,v227);
              const v292 = PLT(v227,DLC_Int 3);
              const v294 = IF_THEN_ELSE(v291,v292,DLC_Bool False);
              const v297 = IF_THEN_ELSE(v289,DLC_Bool False,DLC_Bool True);
              const v299 = IF_THEN_ELSE(v297,DLC_Bool True,v294);
              claim(CT_Assert)(v299);
              const v301 = PEQ(v266,DLC_Int 0);
              const v303 = PLE(DLC_Int 0,v180);
              const v304 = PLT(v180,DLC_Int 3);
              const v306 = IF_THEN_ELSE(v303,v304,DLC_Bool False);
              const v309 = IF_THEN_ELSE(v301,DLC_Bool False,DLC_Bool True);
              const v311 = IF_THEN_ELSE(v309,DLC_Bool True,v306);
              claim(CT_Assert)(v311);
              const v314 = PEQ(v266,DLC_Int 2);
              const v318 = PEQ(v227,DLC_Int 0);
              const v320 = IF_THEN_ELSE(v318,v314,DLC_Bool False);
              claim(CT_Possible)(v320);
              const v323 = PEQ(v227,DLC_Int 1);
              const v325 = IF_THEN_ELSE(v323,v314,DLC_Bool False);
              claim(CT_Possible)(v325);
              const v328 = PEQ(v227,DLC_Int 2);
              const v330 = IF_THEN_ELSE(v328,v314,DLC_Bool False);
              claim(CT_Possible)(v330);
              const v332 = PEQ(v266,DLC_Int 0);
              const v336 = PEQ(v180,DLC_Int 0);
              const v338 = IF_THEN_ELSE(v336,v332,DLC_Bool False);
              claim(CT_Possible)(v338);
              const v341 = PEQ(v180,DLC_Int 1);
              const v343 = IF_THEN_ELSE(v341,v332,DLC_Bool False);
              claim(CT_Possible)(v343);
              const v346 = PEQ(v180,DLC_Int 2);
              const v348 = IF_THEN_ELSE(v346,v332,DLC_Bool False);
              claim(CT_Possible)(v348);
              const v349 = ADD(DLC_Int 1,v75);
              {
                v75 = v349,
                v76 = v266}
              continue; } } } }
      const v352 = PEQ(v76,DLC_Int 1);
      const v354 = IF_THEN_ELSE(v352,DLC_Bool False,DLC_Bool True);
      claim(CT_Assert)(v354);
      let v356;
      const v358 = PEQ(v76,DLC_Int 2);
      if v358 then {
        const v359 = MUL(DLC_Int 2,v5);
        v356 = [v359,DLC_Int 0];
         }
      else {
        const v361 = PEQ(v76,DLC_Int 0);
        if v361 then {
          const v362 = MUL(DLC_Int 2,v5);
          v356 = [DLC_Int 0,v362];
           }
        else {
          v356 = [v5,v5];
           };
         };
      const v363 = v356[0];
      const v364 = v356[1];
      const v365 = ADD(v6,v363);
      transfer.(v365).to(v7);
      transfer.(v364).to(v16);
      commit();
      only("A") {
        let v369;
        const v371 = PLE(DLC_Int 0,v76);
        const v372 = PLT(v76,DLC_Int 5);
        const v374 = IF_THEN_ELSE(v371,v372,DLC_Bool False);
        claim(CT_Require)(v374);
        const v376 = PEQ(v76,DLC_Int 0);
        if v376 then {
          v369 = DLC_Bytes "Bob wins";
           }
        else {
          const v378 = PEQ(v76,DLC_Int 1);
          if v378 then {
            v369 = DLC_Bytes "Draw";
             }
          else {
            const v380 = PEQ(v76,DLC_Int 2);
            if v380 then {
              v369 = DLC_Bytes "Alice wins";
               }
            else {
              const v382 = PEQ(v76,DLC_Int 3);
              if v382 then {
                v369 = DLC_Bytes "Alice quits";
                 }
              else {
                v369 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v383 = interact("A")."endsWith"(v369);
         };
      only("B") {
        let v385;
        const v387 = PLE(DLC_Int 0,v76);
        const v388 = PLT(v76,DLC_Int 5);
        const v390 = IF_THEN_ELSE(v387,v388,DLC_Bool False);
        claim(CT_Require)(v390);
        const v392 = PEQ(v76,DLC_Int 0);
        if v392 then {
          v385 = DLC_Bytes "Bob wins";
           }
        else {
          const v394 = PEQ(v76,DLC_Int 1);
          if v394 then {
            v385 = DLC_Bytes "Draw";
             }
          else {
            const v396 = PEQ(v76,DLC_Int 2);
            if v396 then {
              v385 = DLC_Bytes "Alice wins";
               }
            else {
              const v398 = PEQ(v76,DLC_Int 3);
              if v398 then {
                v385 = DLC_Bytes "Alice quits";
                 }
              else {
                v385 = DLC_Bytes "Bob quits";
                 };
               };
             };
           };
        const v399 = interact("B")."endsWith"(v385);
         };
      exit(); } } }