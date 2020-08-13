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
  const v1 = interact("A")."getParams"();
  const v2 = v1[0];
  const v3 = v1[1];
   };
only("A") {
  const v7 = ADD(v2,v3);
   };
publish("A", join(v6))(v2,v3)(v4, v5).pay(v7){
  const v8 = ADD(v4,v5);
  const v9 = TXN_VALUE();
  const v10 = PEQ(v8,v9);
  claim(CT_Require)(v10);
  commit();
  only("B") {
    const v12 = interact("B")."partnerIs"(v6);
    const v13 = interact("B")."acceptParams"(v4,v5);
     };
  only("B") {
     };
  publish("B", join(v14))()().pay(v4).timeout((DLC_Int 10, {
    only("A") {
       };
    publish("A", again(v6))()().pay(DLC_Int 0){
      const v19 = TXN_VALUE();
      const v20 = PEQ(DLC_Int 0,v19);
      claim(CT_Require)(v20);
      const v21 = BALANCE();
      transfer.(v21).to(v6);
      commit();
      only("A") {
        claim(CT_Require)(DLC_Bool True);
        const v27 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
         };
      only("B") {
        claim(CT_Require)(DLC_Bool True);
        const v32 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
         };
      exit(); } })){
    const v15 = TXN_VALUE();
    const v16 = PEQ(v4,v15);
    claim(CT_Require)(v16);
    commit();
    only("A") {
      const v34 = interact("A")."partnerIs"(v14);
      let v36;
      const v37 = interact("A")."getHand"();
      const v38 = BYTES_EQ(v37,DLC_Bytes "ROCK");
      const v39 = BYTES_EQ(v37,DLC_Bytes "PAPER");
      const v40 = BYTES_EQ(v37,DLC_Bytes "SCISSORS");
      const v42 = IF_THEN_ELSE(v38,DLC_Bool True,v39);
      const v44 = IF_THEN_ELSE(v42,DLC_Bool True,v40);
      claim(CT_Assume)(v44);
      if v38 then {
        v36 = DLC_Int 0;
         }
      else {
        if v39 then {
          v36 = DLC_Int 1;
           }
        else {
          v36 = DLC_Int 2;
           };
         };
      const v47 = PLE(DLC_Int 0,v36);
      const v48 = PLT(v36,DLC_Int 3);
      const v50 = IF_THEN_ELSE(v47,v48,DLC_Bool False);
      claim(CT_Assert)(v50);
      const v52 = interact("A")."random"();
      const v53 = digest(v52,v36);
      const v54 = interact("A")."commits"();
       };
    only("A") {
       };
    publish("A", again(v6))(v53)(v55).pay(DLC_Int 0).timeout((DLC_Int 10, {
      only("B") {
         };
      publish("B", again(v14))()().pay(DLC_Int 0){
        const v60 = TXN_VALUE();
        const v61 = PEQ(DLC_Int 0,v60);
        claim(CT_Require)(v61);
        const v62 = BALANCE();
        transfer.(v62).to(v14);
        commit();
        only("A") {
          claim(CT_Require)(DLC_Bool True);
          const v68 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
           };
        only("B") {
          claim(CT_Require)(DLC_Bool True);
          const v73 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
           };
        exit(); } })){
      const v56 = TXN_VALUE();
      const v57 = PEQ(DLC_Int 0,v56);
      claim(CT_Require)(v57);
      commit();
      only("B") {
        let v76;
        const v77 = interact("B")."getHand"();
        const v78 = BYTES_EQ(v77,DLC_Bytes "ROCK");
        const v79 = BYTES_EQ(v77,DLC_Bytes "PAPER");
        const v80 = BYTES_EQ(v77,DLC_Bytes "SCISSORS");
        const v82 = IF_THEN_ELSE(v78,DLC_Bool True,v79);
        const v84 = IF_THEN_ELSE(v82,DLC_Bool True,v80);
        claim(CT_Assume)(v84);
        if v78 then {
          v76 = DLC_Int 0;
           }
        else {
          if v79 then {
            v76 = DLC_Int 1;
             }
          else {
            v76 = DLC_Int 2;
             };
           };
        const v87 = PLE(DLC_Int 0,v76);
        const v88 = PLT(v76,DLC_Int 3);
        const v90 = IF_THEN_ELSE(v87,v88,DLC_Bool False);
        claim(CT_Assert)(v90);
        const v91 = interact("B")."shows"();
         };
      only("B") {
         };
      publish("B", again(v14))(v76)(v92).pay(DLC_Int 0).timeout((DLC_Int 10, {
        only("A") {
           };
        publish("A", again(v6))()().pay(DLC_Int 0){
          const v97 = TXN_VALUE();
          const v98 = PEQ(DLC_Int 0,v97);
          claim(CT_Require)(v98);
          const v99 = BALANCE();
          transfer.(v99).to(v6);
          commit();
          only("A") {
            claim(CT_Require)(DLC_Bool True);
            const v105 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
             };
          only("B") {
            claim(CT_Require)(DLC_Bool True);
            const v110 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
             };
          exit(); } })){
        const v93 = TXN_VALUE();
        const v94 = PEQ(DLC_Int 0,v93);
        claim(CT_Require)(v94);
        const v112 = PLE(DLC_Int 0,v92);
        const v113 = PLT(v92,DLC_Int 3);
        const v115 = IF_THEN_ELSE(v112,v113,DLC_Bool False);
        claim(CT_Require)(v115);
        commit();
        only("A") {
          let v117;
          const v119 = PLE(DLC_Int 0,v92);
          const v120 = PLT(v92,DLC_Int 3);
          const v122 = IF_THEN_ELSE(v119,v120,DLC_Bool False);
          claim(CT_Require)(v122);
          const v123 = PEQ(v92,DLC_Int 0);
          if v123 then {
            v117 = DLC_Bytes "ROCK";
             }
          else {
            const v124 = PEQ(v92,DLC_Int 1);
            if v124 then {
              v117 = DLC_Bytes "PAPER";
               }
            else {
              v117 = DLC_Bytes "SCISSORS";
               };
             };
          const v125 = interact("A")."reveals"(v117);
           };
        only("A") {
           };
        publish("A", again(v6))(v52,v36)(v126, v127).pay(DLC_Int 0).timeout((DLC_Int 10, {
          only("B") {
             };
          publish("B", again(v14))()().pay(DLC_Int 0){
            const v132 = TXN_VALUE();
            const v133 = PEQ(DLC_Int 0,v132);
            claim(CT_Require)(v133);
            const v134 = BALANCE();
            transfer.(v134).to(v14);
            commit();
            only("A") {
              claim(CT_Require)(DLC_Bool True);
              const v140 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
               };
            only("B") {
              claim(CT_Require)(DLC_Bool True);
              const v145 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
               };
            exit(); } })){
          const v128 = TXN_VALUE();
          const v129 = PEQ(DLC_Int 0,v128);
          claim(CT_Require)(v129);
          const v147 = digest(v126,v127);
          const v148 = PEQ(v55,v147);
          claim(CT_Require)(v148);
          const v150 = PLE(DLC_Int 0,v127);
          const v151 = PLT(v127,DLC_Int 3);
          const v153 = IF_THEN_ELSE(v150,v151,DLC_Bool False);
          claim(CT_Require)(v153);
          let v155;
          const v157 = PLE(DLC_Int 0,v127);
          const v158 = PLT(v127,DLC_Int 3);
          const v160 = IF_THEN_ELSE(v157,v158,DLC_Bool False);
          const v162 = PLE(DLC_Int 0,v92);
          const v163 = PLT(v92,DLC_Int 3);
          const v165 = IF_THEN_ELSE(v162,v163,DLC_Bool False);
          const v167 = IF_THEN_ELSE(v160,v165,DLC_Bool False);
          if v167 then {
            const v168 = SUB(DLC_Int 4,v92);
            const v169 = ADD(v127,v168);
            const v170 = MOD(v169,DLC_Int 3);
            v155 = v170;
             }
          else {
            if v160 then {
              v155 = DLC_Int 2;
               }
            else {
              if v165 then {
                v155 = DLC_Int 0;
                 }
              else {
                v155 = DLC_Int 1;
                 };
               };
             };
          const v173 = PLE(DLC_Int 0,v155);
          const v174 = PLT(v155,DLC_Int 5);
          const v176 = IF_THEN_ELSE(v173,v174,DLC_Bool False);
          claim(CT_Assert)(v176);
          const v177 = PEQ(v155,DLC_Int 2);
          const v179 = PLE(DLC_Int 0,v127);
          const v180 = PLT(v127,DLC_Int 3);
          const v182 = IF_THEN_ELSE(v179,v180,DLC_Bool False);
          const v185 = IF_THEN_ELSE(v177,DLC_Bool False,DLC_Bool True);
          const v187 = IF_THEN_ELSE(v185,DLC_Bool True,v182);
          claim(CT_Assert)(v187);
          const v188 = PEQ(v155,DLC_Int 0);
          const v190 = PLE(DLC_Int 0,v92);
          const v191 = PLT(v92,DLC_Int 3);
          const v193 = IF_THEN_ELSE(v190,v191,DLC_Bool False);
          const v196 = IF_THEN_ELSE(v188,DLC_Bool False,DLC_Bool True);
          const v198 = IF_THEN_ELSE(v196,DLC_Bool True,v193);
          claim(CT_Assert)(v198);
          const v200 = PEQ(v155,DLC_Int 2);
          const v203 = PEQ(v127,DLC_Int 0);
          const v205 = IF_THEN_ELSE(v203,v200,DLC_Bool False);
          claim(CT_Possible)(v205);
          const v207 = PEQ(v127,DLC_Int 1);
          const v209 = IF_THEN_ELSE(v207,v200,DLC_Bool False);
          claim(CT_Possible)(v209);
          const v211 = PEQ(v127,DLC_Int 2);
          const v213 = IF_THEN_ELSE(v211,v200,DLC_Bool False);
          claim(CT_Possible)(v213);
          const v214 = PEQ(v155,DLC_Int 0);
          const v217 = PEQ(v92,DLC_Int 0);
          const v219 = IF_THEN_ELSE(v217,v214,DLC_Bool False);
          claim(CT_Possible)(v219);
          const v221 = PEQ(v92,DLC_Int 1);
          const v223 = IF_THEN_ELSE(v221,v214,DLC_Bool False);
          claim(CT_Possible)(v223);
          const v225 = PEQ(v92,DLC_Int 2);
          const v227 = IF_THEN_ELSE(v225,v214,DLC_Bool False);
          claim(CT_Possible)(v227);
          let v228;
          const v229 = PEQ(v155,DLC_Int 2);
          if v229 then {
            const v230 = MUL(DLC_Int 2,v4);
            v228 = [v230,DLC_Int 0];
             }
          else {
            const v231 = PEQ(v155,DLC_Int 0);
            if v231 then {
              const v232 = MUL(DLC_Int 2,v4);
              v228 = [DLC_Int 0,v232];
               }
            else {
              v228 = [v4,v4];
               };
             };
          const v233 = v228[0];
          const v234 = v228[1];
          const v235 = ADD(v5,v233);
          transfer.(v235).to(v6);
          transfer.(v234).to(v14);
          commit();
          only("A") {
            let v239;
            const v241 = PLE(DLC_Int 0,v155);
            const v242 = PLT(v155,DLC_Int 5);
            const v244 = IF_THEN_ELSE(v241,v242,DLC_Bool False);
            claim(CT_Require)(v244);
            const v245 = PEQ(v155,DLC_Int 0);
            if v245 then {
              v239 = DLC_Bytes "Bob wins";
               }
            else {
              const v246 = PEQ(v155,DLC_Int 1);
              if v246 then {
                v239 = DLC_Bytes "Draw";
                 }
              else {
                const v247 = PEQ(v155,DLC_Int 2);
                if v247 then {
                  v239 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v248 = PEQ(v155,DLC_Int 3);
                  if v248 then {
                    v239 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v239 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v249 = interact("A")."endsWith"(v239);
             };
          only("B") {
            let v251;
            const v253 = PLE(DLC_Int 0,v155);
            const v254 = PLT(v155,DLC_Int 5);
            const v256 = IF_THEN_ELSE(v253,v254,DLC_Bool False);
            claim(CT_Require)(v256);
            const v257 = PEQ(v155,DLC_Int 0);
            if v257 then {
              v251 = DLC_Bytes "Bob wins";
               }
            else {
              const v258 = PEQ(v155,DLC_Int 1);
              if v258 then {
                v251 = DLC_Bytes "Draw";
                 }
              else {
                const v259 = PEQ(v155,DLC_Int 2);
                if v259 then {
                  v251 = DLC_Bytes "Alice wins";
                   }
                else {
                  const v260 = PEQ(v155,DLC_Int 3);
                  if v260 then {
                    v251 = DLC_Bytes "Alice quits";
                     }
                  else {
                    v251 = DLC_Bytes "Bob quits";
                     };
                   };
                 };
               };
            const v261 = interact("B")."endsWith"(v251);
             };
          exit(); } } } } }