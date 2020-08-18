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
  const v11 = PEQ(v9,v10);
  claim(CT_Require)(v11);
  commit();
  only("B") {
    const v13 = interact("B")."partnerIs"(v7);
    const v14 = interact("B")."acceptParams"(v5,v6);
     };
  only("B") {
     };
  publish("B", join(v15))()().pay(v5)
    .timeout((DLC_Int 10, {
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
      let v39;
      const v40 = interact("A")."getHand"();
      const v41 = BYTES_EQ(v40,DLC_Bytes "ROCK");
      const v42 = BYTES_EQ(v40,DLC_Bytes "PAPER");
      const v43 = BYTES_EQ(v40,DLC_Bytes "SCISSORS");
      const v45 = IF_THEN_ELSE(v41,DLC_Bool True,v42);
      const v47 = IF_THEN_ELSE(v45,DLC_Bool True,v43);
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
      const v50 = PLE(DLC_Int 0,v39);
      const v51 = PLT(v39,DLC_Int 3);
      const v53 = IF_THEN_ELSE(v50,v51,DLC_Bool False);
      claim(CT_Assert)(v53);
      const v55 = interact("A")."random"();
      const v56 = digest(v55,v39);
      const v57 = interact("A")."commits"();
       };
    only("A") {
       };
    publish("A", again(v7))(v56)(v58).pay(DLC_Int 0)
      .timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again(v15))()().pay(DLC_Int 0){
          const v63 = TXN_VALUE();
          const v64 = PEQ(DLC_Int 0,v63);
          claim(CT_Require)(v64);
          const v65 = BALANCE();
          transfer.(v65).to(v15);
          commit();
          only("A") {
            claim(CT_Require)(DLC_Bool True);
            const v71 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
             };
          only("B") {
            claim(CT_Require)(DLC_Bool True);
            const v76 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
             };
          exit(); } })){
      const v59 = TXN_VALUE();
      const v60 = PEQ(DLC_Int 0,v59);
      claim(CT_Require)(v60);
      commit();
      only("B") {
        let v79;
        const v80 = interact("B")."getHand"();
        const v81 = BYTES_EQ(v80,DLC_Bytes "ROCK");
        const v82 = BYTES_EQ(v80,DLC_Bytes "PAPER");
        const v83 = BYTES_EQ(v80,DLC_Bytes "SCISSORS");
        const v85 = IF_THEN_ELSE(v81,DLC_Bool True,v82);
        const v87 = IF_THEN_ELSE(v85,DLC_Bool True,v83);
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
        const v90 = PLE(DLC_Int 0,v79);
        const v91 = PLT(v79,DLC_Int 3);
        const v93 = IF_THEN_ELSE(v90,v91,DLC_Bool False);
        claim(CT_Assert)(v93);
        const v94 = interact("B")."shows"();
         };
      only("B") {
         };
      publish("B", again(v15))(v79)(v95).pay(DLC_Int 0)
        .timeout((DLC_Int 10, {
          only("A") {
             };
          publish("A", again(v7))()().pay(DLC_Int 0){
            const v100 = TXN_VALUE();
            const v101 = PEQ(DLC_Int 0,v100);
            claim(CT_Require)(v101);
            const v102 = BALANCE();
            transfer.(v102).to(v7);
            commit();
            only("A") {
              claim(CT_Require)(DLC_Bool True);
              const v108 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
               };
            only("B") {
              claim(CT_Require)(DLC_Bool True);
              const v113 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
               };
            exit(); } })){
        const v96 = TXN_VALUE();
        const v97 = PEQ(DLC_Int 0,v96);
        claim(CT_Require)(v97);
        const v115 = PLE(DLC_Int 0,v95);
        const v116 = PLT(v95,DLC_Int 3);
        const v118 = IF_THEN_ELSE(v115,v116,DLC_Bool False);
        claim(CT_Require)(v118);
        commit();
        only("A") {
          let v120;
          const v122 = PLE(DLC_Int 0,v95);
          const v123 = PLT(v95,DLC_Int 3);
          const v125 = IF_THEN_ELSE(v122,v123,DLC_Bool False);
          claim(CT_Require)(v125);
          const v126 = PEQ(v95,DLC_Int 0);
          if v126 then {
            v120 = DLC_Bytes "ROCK";
             }
          else {
            const v127 = PEQ(v95,DLC_Int 1);
            if v127 then {
              v120 = DLC_Bytes "PAPER";
               }
            else {
              v120 = DLC_Bytes "SCISSORS";
               };
             };
          const v128 = interact("A")."reveals"(v120);
           };
        only("A") {
           };
        publish("A", again(v7))(v55,v39)(v129, v130).pay(DLC_Int 0)
          .timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again(v15))()().pay(DLC_Int 0){
              const v135 = TXN_VALUE();
              const v136 = PEQ(DLC_Int 0,v135);
              claim(CT_Require)(v136);
              const v137 = BALANCE();
              transfer.(v137).to(v15);
              commit();
              only("A") {
                claim(CT_Require)(DLC_Bool True);
                const v143 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                 };
              only("B") {
                claim(CT_Require)(DLC_Bool True);
                const v148 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                 };
              exit(); } })){
          const v131 = TXN_VALUE();
          const v132 = PEQ(DLC_Int 0,v131);
          claim(CT_Require)(v132);
          const v150 = digest(v129,v130);
          const v151 = PEQ(v58,v150);
          claim(CT_Require)(v151);
          const v153 = PLE(DLC_Int 0,v130);
          const v154 = PLT(v130,DLC_Int 3);
          const v156 = IF_THEN_ELSE(v153,v154,DLC_Bool False);
          claim(CT_Require)(v156);
          let v158;
          const v160 = PLE(DLC_Int 0,v130);
          const v161 = PLT(v130,DLC_Int 3);
          const v163 = IF_THEN_ELSE(v160,v161,DLC_Bool False);
          const v165 = PLE(DLC_Int 0,v95);
          const v166 = PLT(v95,DLC_Int 3);
          const v168 = IF_THEN_ELSE(v165,v166,DLC_Bool False);
          const v170 = IF_THEN_ELSE(v163,v168,DLC_Bool False);
          if v170 then {
            const v171 = SUB(DLC_Int 4,v95);
            const v172 = ADD(v130,v171);
            const v173 = MOD(v172,DLC_Int 3);
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
          const v176 = PLE(DLC_Int 0,v158);
          const v177 = PLT(v158,DLC_Int 5);
          const v179 = IF_THEN_ELSE(v176,v177,DLC_Bool False);
          claim(CT_Assert)(v179);
          const v180 = PEQ(v158,DLC_Int 2);
          const v182 = PLE(DLC_Int 0,v130);
          const v183 = PLT(v130,DLC_Int 3);
          const v185 = IF_THEN_ELSE(v182,v183,DLC_Bool False);
          const v188 = IF_THEN_ELSE(v180,DLC_Bool False,DLC_Bool True);
          const v190 = IF_THEN_ELSE(v188,DLC_Bool True,v185);
          claim(CT_Assert)(v190);
          const v191 = PEQ(v158,DLC_Int 0);
          const v193 = PLE(DLC_Int 0,v95);
          const v194 = PLT(v95,DLC_Int 3);
          const v196 = IF_THEN_ELSE(v193,v194,DLC_Bool False);
          const v199 = IF_THEN_ELSE(v191,DLC_Bool False,DLC_Bool True);
          const v201 = IF_THEN_ELSE(v199,DLC_Bool True,v196);
          claim(CT_Assert)(v201);
          const v203 = PEQ(v158,DLC_Int 2);
          const v206 = PEQ(v130,DLC_Int 0);
          const v208 = IF_THEN_ELSE(v206,v203,DLC_Bool False);
          claim(CT_Possible)(v208);
          const v210 = PEQ(v130,DLC_Int 1);
          const v212 = IF_THEN_ELSE(v210,v203,DLC_Bool False);
          claim(CT_Possible)(v212);
          const v214 = PEQ(v130,DLC_Int 2);
          const v216 = IF_THEN_ELSE(v214,v203,DLC_Bool False);
          claim(CT_Possible)(v216);
          const v217 = PEQ(v158,DLC_Int 0);
          const v220 = PEQ(v95,DLC_Int 0);
          const v222 = IF_THEN_ELSE(v220,v217,DLC_Bool False);
          claim(CT_Possible)(v222);
          const v224 = PEQ(v95,DLC_Int 1);
          const v226 = IF_THEN_ELSE(v224,v217,DLC_Bool False);
          claim(CT_Possible)(v226);
          const v228 = PEQ(v95,DLC_Int 2);
          const v230 = IF_THEN_ELSE(v228,v217,DLC_Bool False);
          claim(CT_Possible)(v230);
          let v232;
          const v233 = PEQ(v158,DLC_Int 2);
          if v233 then {
            const v234 = MUL(DLC_Int 2,v5);
            v232 = [v234,DLC_Int 0];
             }
          else {
            const v235 = PEQ(v158,DLC_Int 0);
            if v235 then {
              const v236 = MUL(DLC_Int 2,v5);
              v232 = [DLC_Int 0,v236];
               }
            else {
              v232 = [v5,v5];
               };
             };
          const v237 = v232[0];
          const v238 = v232[1];
          const v239 = ADD(v6,v237);
          transfer.(v239).to(v7);
          transfer.(v238).to(v15);
          commit();
          only("A") {
            let v243;
            const v245 = PLE(DLC_Int 0,v158);
            const v246 = PLT(v158,DLC_Int 5);
            const v248 = IF_THEN_ELSE(v245,v246,DLC_Bool False);
            claim(CT_Require)(v248);
            const v249 = PEQ(v158,DLC_Int 0);
            if v249 then {
              v243 = DLC_Bytes "Bob wins";
               }
            else {
              const v250 = PEQ(v158,DLC_Int 1);
              if v250 then {
                v243 = DLC_Bytes "Draw";
                 }
              else {
                const v251 = PEQ(v158,DLC_Int 2);
                if v251 then {
                  v243 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v252 = PEQ(v158,DLC_Int 3);
                  if v252 then {
                    v243 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v243 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v253 = interact("A")."endsWith"(v243);
             };
          only("B") {
            let v255;
            const v257 = PLE(DLC_Int 0,v158);
            const v258 = PLT(v158,DLC_Int 5);
            const v260 = IF_THEN_ELSE(v257,v258,DLC_Bool False);
            claim(CT_Require)(v260);
            const v261 = PEQ(v158,DLC_Int 0);
            if v261 then {
              v255 = DLC_Bytes "Bob wins";
               }
            else {
              const v262 = PEQ(v158,DLC_Int 1);
              if v262 then {
                v255 = DLC_Bytes "Draw";
                 }
              else {
                const v263 = PEQ(v158,DLC_Int 2);
                if v263 then {
                  v255 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v264 = PEQ(v158,DLC_Int 3);
                  if v264 then {
                    v255 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v255 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v265 = interact("B")."endsWith"(v255);
             };
          exit(); } } } } }