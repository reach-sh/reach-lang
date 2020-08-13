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
    const v12 = interact("B")."acceptParams"(v4,v5);
     };
  only("B") {
     };
  publish("B", join(v13))()().pay(v4).timeout((DLC_Int 10, {
    only("A") {
       };
    publish("A", again(v6))()().pay(DLC_Int 0){
      const v18 = TXN_VALUE();
      const v19 = PEQ(DLC_Int 0,v18);
      claim(CT_Require)(v19);
      const v20 = BALANCE();
      transfer.(v20).to(v6);
      commit();
      only("A") {
        claim(CT_Require)(DLC_Bool True);
        const v26 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
         };
      only("B") {
        claim(CT_Require)(DLC_Bool True);
        const v31 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
         };
      exit(); } })){
    const v14 = TXN_VALUE();
    const v15 = PEQ(v4,v14);
    claim(CT_Require)(v15);
    loopvar {
      v32 = DLC_Int 0,
      v33 = DLC_Int 1};
    invariant{
      const v34 = BALANCE();
      const v35 = MUL(DLC_Int 2,v4);
      const v36 = ADD(v35,v5);
      const v37 = PEQ(v34,v36);
      const v39 = PLE(DLC_Int 0,v33);
      const v40 = PLT(v33,DLC_Int 5);
      const v42 = IF_THEN_ELSE(v39,v40,DLC_Bool False);
      const v44 = IF_THEN_ELSE(v37,v42,DLC_Bool False);
      const v46 = PEQ(v33,DLC_Int 3);
      const v48 = IF_THEN_ELSE(v46,DLC_Bool False,DLC_Bool True);
      const v50 = IF_THEN_ELSE(v44,v48,DLC_Bool False);
      const v52 = PEQ(v33,DLC_Int 4);
      const v54 = IF_THEN_ELSE(v52,DLC_Bool False,DLC_Bool True);
      const v56 = IF_THEN_ELSE(v50,v54,DLC_Bool False);
      
      return v56; }
    while{
      const v57 = PEQ(v33,DLC_Int 1);
      
      return v57; }
    {
      commit();
      only("A") {
        let v60;
        const v61 = interact("A")."getHand"();
        const v62 = BYTES_EQ(v61,DLC_Bytes "ROCK");
        const v63 = BYTES_EQ(v61,DLC_Bytes "PAPER");
        const v64 = BYTES_EQ(v61,DLC_Bytes "SCISSORS");
        const v66 = IF_THEN_ELSE(v62,DLC_Bool True,v63);
        const v68 = IF_THEN_ELSE(v66,DLC_Bool True,v64);
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
        const v71 = PLE(DLC_Int 0,v60);
        const v72 = PLT(v60,DLC_Int 3);
        const v74 = IF_THEN_ELSE(v71,v72,DLC_Bool False);
        claim(CT_Assert)(v74);
        const v76 = interact("A")."random"();
        const v77 = digest(v76,v60);
        const v78 = interact("A")."commits"();
         };
      only("A") {
         };
      publish("A", again(v6))(v77)(v79).pay(DLC_Int 0).timeout((DLC_Int 10, {
        only("B") {
           };
        publish("B", again(v13))()().pay(DLC_Int 0){
          const v84 = TXN_VALUE();
          const v85 = PEQ(DLC_Int 0,v84);
          claim(CT_Require)(v85);
          const v86 = BALANCE();
          transfer.(v86).to(v13);
          commit();
          only("A") {
            claim(CT_Require)(DLC_Bool True);
            const v92 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
             };
          only("B") {
            claim(CT_Require)(DLC_Bool True);
            const v97 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
             };
          exit(); } })){
        const v80 = TXN_VALUE();
        const v81 = PEQ(DLC_Int 0,v80);
        claim(CT_Require)(v81);
        commit();
        only("B") {
          let v100;
          const v101 = interact("B")."getHand"();
          const v102 = BYTES_EQ(v101,DLC_Bytes "ROCK");
          const v103 = BYTES_EQ(v101,DLC_Bytes "PAPER");
          const v104 = BYTES_EQ(v101,DLC_Bytes "SCISSORS");
          const v106 = IF_THEN_ELSE(v102,DLC_Bool True,v103);
          const v108 = IF_THEN_ELSE(v106,DLC_Bool True,v104);
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
          const v111 = PLE(DLC_Int 0,v100);
          const v112 = PLT(v100,DLC_Int 3);
          const v114 = IF_THEN_ELSE(v111,v112,DLC_Bool False);
          claim(CT_Assert)(v114);
          const v115 = interact("B")."shows"();
           };
        only("B") {
           };
        publish("B", again(v13))(v100)(v116).pay(DLC_Int 0).timeout((DLC_Int 10, {
          only("A") {
             };
          publish("A", again(v6))()().pay(DLC_Int 0){
            const v121 = TXN_VALUE();
            const v122 = PEQ(DLC_Int 0,v121);
            claim(CT_Require)(v122);
            const v123 = BALANCE();
            transfer.(v123).to(v6);
            commit();
            only("A") {
              claim(CT_Require)(DLC_Bool True);
              const v129 = interact("A")."endsWith"(DLC_Bytes "Bob quits");
               };
            only("B") {
              claim(CT_Require)(DLC_Bool True);
              const v134 = interact("B")."endsWith"(DLC_Bytes "Bob quits");
               };
            exit(); } })){
          const v117 = TXN_VALUE();
          const v118 = PEQ(DLC_Int 0,v117);
          claim(CT_Require)(v118);
          const v136 = PLE(DLC_Int 0,v116);
          const v137 = PLT(v116,DLC_Int 3);
          const v139 = IF_THEN_ELSE(v136,v137,DLC_Bool False);
          claim(CT_Require)(v139);
          commit();
          only("A") {
            let v141;
            const v143 = PLE(DLC_Int 0,v116);
            const v144 = PLT(v116,DLC_Int 3);
            const v146 = IF_THEN_ELSE(v143,v144,DLC_Bool False);
            claim(CT_Require)(v146);
            const v147 = PEQ(v116,DLC_Int 0);
            if v147 then {
              v141 = DLC_Bytes "ROCK";
               }
            else {
              const v148 = PEQ(v116,DLC_Int 1);
              if v148 then {
                v141 = DLC_Bytes "PAPER";
                 }
              else {
                v141 = DLC_Bytes "SCISSORS";
                 };
               };
            const v149 = interact("A")."reveals"(v141);
             };
          only("A") {
             };
          publish("A", again(v6))(v76,v60)(v150, v151).pay(DLC_Int 0).timeout((DLC_Int 10, {
            only("B") {
               };
            publish("B", again(v13))()().pay(DLC_Int 0){
              const v156 = TXN_VALUE();
              const v157 = PEQ(DLC_Int 0,v156);
              claim(CT_Require)(v157);
              const v158 = BALANCE();
              transfer.(v158).to(v13);
              commit();
              only("A") {
                claim(CT_Require)(DLC_Bool True);
                const v164 = interact("A")."endsWith"(DLC_Bytes "Alice quits");
                 };
              only("B") {
                claim(CT_Require)(DLC_Bool True);
                const v169 = interact("B")."endsWith"(DLC_Bytes "Alice quits");
                 };
              exit(); } })){
            const v152 = TXN_VALUE();
            const v153 = PEQ(DLC_Int 0,v152);
            claim(CT_Require)(v153);
            const v171 = digest(v150,v151);
            const v172 = PEQ(v79,v171);
            claim(CT_Require)(v172);
            const v174 = PLE(DLC_Int 0,v151);
            const v175 = PLT(v151,DLC_Int 3);
            const v177 = IF_THEN_ELSE(v174,v175,DLC_Bool False);
            claim(CT_Require)(v177);
            let v179;
            const v181 = PLE(DLC_Int 0,v151);
            const v182 = PLT(v151,DLC_Int 3);
            const v184 = IF_THEN_ELSE(v181,v182,DLC_Bool False);
            const v186 = PLE(DLC_Int 0,v116);
            const v187 = PLT(v116,DLC_Int 3);
            const v189 = IF_THEN_ELSE(v186,v187,DLC_Bool False);
            const v191 = IF_THEN_ELSE(v184,v189,DLC_Bool False);
            if v191 then {
              const v192 = SUB(DLC_Int 4,v116);
              const v193 = ADD(v151,v192);
              const v194 = MOD(v193,DLC_Int 3);
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
            const v197 = PLE(DLC_Int 0,v179);
            const v198 = PLT(v179,DLC_Int 5);
            const v200 = IF_THEN_ELSE(v197,v198,DLC_Bool False);
            claim(CT_Assert)(v200);
            const v201 = PEQ(v179,DLC_Int 2);
            const v203 = PLE(DLC_Int 0,v151);
            const v204 = PLT(v151,DLC_Int 3);
            const v206 = IF_THEN_ELSE(v203,v204,DLC_Bool False);
            const v209 = IF_THEN_ELSE(v201,DLC_Bool False,DLC_Bool True);
            const v211 = IF_THEN_ELSE(v209,DLC_Bool True,v206);
            claim(CT_Assert)(v211);
            const v212 = PEQ(v179,DLC_Int 0);
            const v214 = PLE(DLC_Int 0,v116);
            const v215 = PLT(v116,DLC_Int 3);
            const v217 = IF_THEN_ELSE(v214,v215,DLC_Bool False);
            const v220 = IF_THEN_ELSE(v212,DLC_Bool False,DLC_Bool True);
            const v222 = IF_THEN_ELSE(v220,DLC_Bool True,v217);
            claim(CT_Assert)(v222);
            const v224 = PEQ(v179,DLC_Int 2);
            const v227 = PEQ(v151,DLC_Int 0);
            const v229 = IF_THEN_ELSE(v227,v224,DLC_Bool False);
            claim(CT_Possible)(v229);
            const v231 = PEQ(v151,DLC_Int 1);
            const v233 = IF_THEN_ELSE(v231,v224,DLC_Bool False);
            claim(CT_Possible)(v233);
            const v235 = PEQ(v151,DLC_Int 2);
            const v237 = IF_THEN_ELSE(v235,v224,DLC_Bool False);
            claim(CT_Possible)(v237);
            const v238 = PEQ(v179,DLC_Int 0);
            const v241 = PEQ(v116,DLC_Int 0);
            const v243 = IF_THEN_ELSE(v241,v238,DLC_Bool False);
            claim(CT_Possible)(v243);
            const v245 = PEQ(v116,DLC_Int 1);
            const v247 = IF_THEN_ELSE(v245,v238,DLC_Bool False);
            claim(CT_Possible)(v247);
            const v249 = PEQ(v116,DLC_Int 2);
            const v251 = IF_THEN_ELSE(v249,v238,DLC_Bool False);
            claim(CT_Possible)(v251);
            const v252 = ADD(DLC_Int 1,v32);
            {
              v32 = v252,
              v33 = v179}
            continue; } } } }
    const v254 = PEQ(v33,DLC_Int 1);
    const v256 = IF_THEN_ELSE(v254,DLC_Bool False,DLC_Bool True);
    claim(CT_Assert)(v256);
    let v257;
    const v258 = PEQ(v33,DLC_Int 2);
    if v258 then {
      const v259 = MUL(DLC_Int 2,v4);
      v257 = [v259,DLC_Int 0];
       }
    else {
      const v260 = PEQ(v33,DLC_Int 0);
      if v260 then {
        const v261 = MUL(DLC_Int 2,v4);
        v257 = [DLC_Int 0,v261];
         }
      else {
        v257 = [v4,v4];
         };
       };
    const v262 = v257[0];
    const v263 = v257[1];
    const v264 = ADD(v5,v262);
    transfer.(v264).to(v6);
    transfer.(v263).to(v13);
    commit();
    only("A") {
      let v268;
      const v270 = PLE(DLC_Int 0,v33);
      const v271 = PLT(v33,DLC_Int 5);
      const v273 = IF_THEN_ELSE(v270,v271,DLC_Bool False);
      claim(CT_Require)(v273);
      const v274 = PEQ(v33,DLC_Int 0);
      if v274 then {
        v268 = DLC_Bytes "Bob wins";
         }
      else {
        const v275 = PEQ(v33,DLC_Int 1);
        if v275 then {
          v268 = DLC_Bytes "Draw";
           }
        else {
          const v276 = PEQ(v33,DLC_Int 2);
          if v276 then {
            v268 = DLC_Bytes "Alice wins";
             }
          else {
            const v277 = PEQ(v33,DLC_Int 3);
            if v277 then {
              v268 = DLC_Bytes "Alice quits";
               }
            else {
              v268 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      const v278 = interact("A")."endsWith"(v268);
       };
    only("B") {
      let v280;
      const v282 = PLE(DLC_Int 0,v33);
      const v283 = PLT(v33,DLC_Int 5);
      const v285 = IF_THEN_ELSE(v282,v283,DLC_Bool False);
      claim(CT_Require)(v285);
      const v286 = PEQ(v33,DLC_Int 0);
      if v286 then {
        v280 = DLC_Bytes "Bob wins";
         }
      else {
        const v287 = PEQ(v33,DLC_Int 1);
        if v287 then {
          v280 = DLC_Bytes "Draw";
           }
        else {
          const v288 = PEQ(v33,DLC_Int 2);
          if v288 then {
            v280 = DLC_Bytes "Alice wins";
             }
          else {
            const v289 = PEQ(v33,DLC_Int 3);
            if v289 then {
              v280 = DLC_Bytes "Alice quits";
               }
            else {
              v280 = DLC_Bytes "Bob quits";
               };
             };
           };
         };
      const v290 = interact("B")."endsWith"(v280);
       };
    exit(); } }