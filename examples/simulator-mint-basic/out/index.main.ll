#lang ll
parts {
  "Alice" = interact {
    didTransfer = IT_Fun [Bool,UInt] Null,
    getParams = IT_Fun [] Object({"amt": UInt, "metadata": Bytes(32), "name": Bytes(32), "supply": UInt, "symbol": Bytes(8), "url": Bytes(96)}),
    showToken = IT_UDFun Null},
  "Bob" = interact {
    didTransfer = IT_Fun [Bool,UInt] Null,
    showToken = IT_UDFun Null}};

// maps
{
  }
// initialization

{
  }
{
  }
{
  }
{
  }
const v221* = {
  i = 0,
  sign = true};
const v222* = {
  i = 0,
  sign = true};
only(Left "Alice") {
   };
only(Left "Bob") {
   };
only(Left "Alice") {
  const v223* = selfAddress("Alice", False, 19 )();
  let v224;
  const v225* = protect<Object({"amt": UInt, "metadata": Bytes(32), "name": Bytes(32), "supply": UInt, "symbol": Bytes(8), "url": Bytes(96)})>("Alice".interact.getParams());
  const v226* = v225.name;
  const v227* = v225.symbol;
  const v228* = v225.url;
  const v229* = v225.metadata;
  const v230* = v225.supply;
  const v231* = v225.amt;
  const v232* = 4 * v231;
  const v233* = v232 <= v230;
  claim(CT_Assume False)(v233, Nothing);
  const v234* = 4 * v231;
  const v235* = v234 <= UInt.max;
  claim(CT_Assume False)(v235, Nothing);
  v224 = null;
   };
only(Left "Alice") {
   };
publish(@0)
  .case("Alice").send({
    isClass = False,
    msg = [v226, v227, v228, v229, v230, v231],
    pay = [0, ],
    when = true})
  .recv({
    didSend = v43,
    from = v236,
    msg = [v237, v238, v239, v240, v241, v242],
    secs = v244,
    time = v243}){
    timeOrder((Nothing, thisConsensusTime/243 ), (Nothing, thisConsensusSecs/244 ) );
    const v245* = 0;
    checkPay(0, Nothing);
    const v246* = 4 * amt/242;
    const v247* = v246 <= supply/241;
    claim(CT_Require)(v247, Nothing);
    const v248* = 4 * amt/242;
    const v249* = v248 <= UInt.max;
    claim(CT_Require)(v249, Nothing);
    const v250* = new Token({
      decimals = null,
      metadata = metadata/240,
      name = name/237,
      supply = supply/241,
      sym = symbol/238,
      url = url/239});
    const v251* = emitLog(internal)(v250 );
    only(Left "Alice") {
      const v252* = selfAddress("Alice", False, 51 )();
      const v253* = {
        metadata = metadata/240,
        name = name/237,
        supply = supply/241,
        symbol = symbol/238,
        url = url/239};
      protect<Null>("Alice".interact.showToken(v251, v253 ));
       };
    commit();
    only(Left "Bob") {
       };
    publish(@thisConsensusTime/243)
      .case("Bob").send({
        isClass = False,
        msg = [],
        pay = [0, ],
        when = true})
      .recv({
        didSend = v59,
        from = v254,
        msg = [],
        secs = v256,
        time = v255}){
        timeOrder((Just thisConsensusTime/243, thisConsensusTime/255 ), (Just thisConsensusSecs/244, thisConsensusSecs/256 ) );
        const v257* = balance(0)/245;
        checkPay(0, Nothing);
        only(Left "Bob") {
          const v258* = selfAddress("Bob", False, 61 )();
          const v259* = {
            metadata = metadata/240,
            name = name/237,
            supply = supply/241,
            symbol = symbol/238,
            url = url/239};
          protect<Null>("Bob".interact.showToken(v251, v259 ));
           };
        commit();
        only(Left "Bob") {
           };
        publish(@thisConsensusTime/255)
          .case("Bob").send({
            isClass = False,
            msg = [],
            pay = [0, ],
            when = true})
          .recv({
            didSend = v70,
            from = v260,
            msg = [],
            secs = v262,
            time = v261}){
            timeOrder((Just thisConsensusTime/255, thisConsensusTime/261 ), (Just thisConsensusSecs/256, thisConsensusSecs/262 ) );
            const v263* = balance(0)/257;
            checkPay(0, Nothing);
            const v264* = v254 == v260;
            claim(CT_Require)(v264, Just "sender correct");
            const v265* = 2 * amt/242;
            const v266* = supply/241;
            const v267* = v265 <= balance(1)/266;
            claim(CT_Assert)(v267, Just "balance sufficient for transfer");
            const v268* = supply/241;
            const v269* = balance(1)/268 - v265;
            transfer.(v265, Just v251).to(v254);
            only(Left "Bob") {
              const v270* = selfAddress("Bob", False, 78 )();
              protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
               };
            const v271* = null;
            commit();
            only(Left "Alice") {
               };
            publish(@thisConsensusTime/261)
              .case("Alice").send({
                isClass = False,
                msg = [],
                pay = [0, ],
                when = true})
              .recv({
                didSend = v86,
                from = v272,
                msg = [],
                secs = v274,
                time = v273}){
                timeOrder((Just thisConsensusTime/261, thisConsensusTime/273 ), (Just thisConsensusSecs/262, thisConsensusSecs/274 ) );
                const v275* = balance(0)/263;
                checkPay(0, Nothing);
                const v276* = v236 == v272;
                claim(CT_Require)(v276, Just "sender correct");
                const v277* = 2 * amt/242;
                const v278* = v269;
                const v279* = v277 <= balance(1)/278;
                claim(CT_Assert)(v279, Just "balance sufficient for transfer");
                const v280* = v269;
                const v281* = balance(1)/280 - v277;
                transfer.(v277, Just v251).to(v236);
                only(Left "Alice") {
                  const v282* = selfAddress("Alice", False, 94 )();
                  protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
                   };
                const v283* = null;
                commit();
                only(Left "Alice") {
                  const v284* = 2 * amt/242;
                   };
                publish(@thisConsensusTime/273)
                  .case("Alice").send({
                    isClass = False,
                    msg = [],
                    pay = [0, (v284, v251 ) ],
                    when = true})
                  .recv({
                    didSend = v106,
                    from = v285,
                    msg = [],
                    secs = v287,
                    time = v286}){
                    timeOrder((Just thisConsensusTime/273, thisConsensusTime/286 ), (Just thisConsensusSecs/274, thisConsensusSecs/287 ) );
                    const v288* = 2 * amt/242;
                    const v289* = balance(0)/275;
                    checkPay(0, Nothing);
                    const v290* = v281;
                    const v291* = balance(1)/290 + v288;
                    checkPay(v288, Just v251);
                    const v292* = v236 == v285;
                    claim(CT_Require)(v292, Just "sender correct");
                    commit();
                    only(Left "Bob") {
                      const v293* = 2 * amt/242;
                       };
                    publish(@thisConsensusTime/286)
                      .case("Bob").send({
                        isClass = False,
                        msg = [],
                        pay = [0, (v293, v251 ) ],
                        when = true})
                      .recv({
                        didSend = v116,
                        from = v294,
                        msg = [],
                        secs = v296,
                        time = v295}){
                        timeOrder((Just thisConsensusTime/286, thisConsensusTime/295 ), (Just thisConsensusSecs/287, thisConsensusSecs/296 ) );
                        const v297* = 2 * amt/242;
                        const v298* = balance(0)/289;
                        checkPay(0, Nothing);
                        const v299* = v291;
                        const v300* = balance(1)/299 + v297;
                        checkPay(v297, Just v251);
                        const v301* = v254 == v294;
                        claim(CT_Require)(v301, Just "sender correct");
                        const v302* = v300;
                        const v303* = supply/241 <= balance(1)/302;
                        claim(CT_Assert)(v303, Just "Token.burn");
                        const v304* = v300;
                        const v305* = balance(1)/304 - supply/241;
                        const v306* = supply/241;
                        const v307* = supply(1)/306 - supply/241;
                        Token(v251).burn(supply/241);
                        const v308* = false;
                        const v309* = (destroyed(1)/308 ? false : true);
                        claim(CT_Assert)(v309, Just "token not yet destroyed at Token.destroy");
                        const v310* = v307;
                        const v311* = 0 == supply(1)/310;
                        claim(CT_Assert)(v311, Just "token supply zero at Token.destroy");
                        Token(v251).destroy();
                        const v312* = "                                                                                                ";
                        const v313* = "                                ";
                        const v314* = new Token({
                          decimals = null,
                          metadata = v313,
                          name = name/237,
                          supply = UInt.max,
                          sym = symbol/238,
                          url = v312});
                        const v315* = emitLog(internal)(v314 );
                        only(Left "Alice") {
                          const v316* = selfAddress("Alice", False, 132 )();
                          const v317* = {
                            name = name/237,
                            symbol = symbol/238};
                          protect<Null>("Alice".interact.showToken(v315, v317 ));
                           };
                        only(Left "Bob") {
                          const v318* = selfAddress("Bob", False, 137 )();
                          const v319* = {
                            name = name/237,
                            symbol = symbol/238};
                          protect<Null>("Bob".interact.showToken(v315, v319 ));
                           };
                        commit();
                        only(Left "Bob") {
                           };
                        publish(@thisConsensusTime/295)
                          .case("Bob").send({
                            isClass = False,
                            msg = [],
                            pay = [0, ],
                            when = true})
                          .recv({
                            didSend = v146,
                            from = v320,
                            msg = [],
                            secs = v322,
                            time = v321}){
                            timeOrder((Just thisConsensusTime/295, thisConsensusTime/321 ), (Just thisConsensusSecs/296, thisConsensusSecs/322 ) );
                            const v323* = balance(0)/298;
                            checkPay(0, Nothing);
                            const v324* = v254 == v320;
                            claim(CT_Require)(v324, Just "sender correct");
                            const v325* = 2 * amt/242;
                            const v326* = UInt.max;
                            const v327* = v325 <= balance(2)/326;
                            claim(CT_Assert)(v327, Just "balance sufficient for transfer");
                            const v328* = UInt.max;
                            const v329* = balance(2)/328 - v325;
                            transfer.(v325, Just v315).to(v254);
                            only(Left "Bob") {
                              const v330* = selfAddress("Bob", False, 154 )();
                              protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
                               };
                            const v331* = null;
                            commit();
                            only(Left "Alice") {
                               };
                            publish(@thisConsensusTime/321)
                              .case("Alice").send({
                                isClass = False,
                                msg = [],
                                pay = [0, ],
                                when = true})
                              .recv({
                                didSend = v162,
                                from = v332,
                                msg = [],
                                secs = v334,
                                time = v333}){
                                timeOrder((Just thisConsensusTime/321, thisConsensusTime/333 ), (Just thisConsensusSecs/322, thisConsensusSecs/334 ) );
                                const v335* = balance(0)/323;
                                checkPay(0, Nothing);
                                const v336* = v236 == v332;
                                claim(CT_Require)(v336, Just "sender correct");
                                const v337* = 2 * amt/242;
                                const v338* = v329;
                                const v339* = v337 <= balance(2)/338;
                                claim(CT_Assert)(v339, Just "balance sufficient for transfer");
                                const v340* = v329;
                                const v341* = balance(2)/340 - v337;
                                transfer.(v337, Just v315).to(v236);
                                only(Left "Alice") {
                                  const v342* = selfAddress("Alice", False, 170 )();
                                  protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
                                   };
                                const v343* = null;
                                const v344* = v341;
                                const v345* = v341;
                                const v346* = balance(2)/344 <= balance(2)/345;
                                claim(CT_Assert)(v346, Just "Token.burn");
                                const v347* = v341;
                                const v348* = balance(2)/347 - balance(2)/344;
                                const v349* = UInt.max;
                                const v350* = supply(2)/349 - balance(2)/344;
                                Token(v315).burn(balance(2)/344);
                                commit();
                                only(Left "Alice") {
                                  const v351* = 2 * amt/242;
                                   };
                                publish(@thisConsensusTime/333)
                                  .case("Alice").send({
                                    isClass = False,
                                    msg = [],
                                    pay = [0, (v351, v315 ) ],
                                    when = true})
                                  .recv({
                                    didSend = v189,
                                    from = v352,
                                    msg = [],
                                    secs = v354,
                                    time = v353}){
                                    timeOrder((Just thisConsensusTime/333, thisConsensusTime/353 ), (Just thisConsensusSecs/334, thisConsensusSecs/354 ) );
                                    const v355* = 2 * amt/242;
                                    const v356* = balance(0)/335;
                                    checkPay(0, Nothing);
                                    const v357* = v348;
                                    const v358* = balance(2)/357 + v355;
                                    checkPay(v355, Just v315);
                                    const v359* = v236 == v352;
                                    claim(CT_Require)(v359, Just "sender correct");
                                    commit();
                                    only(Left "Bob") {
                                      const v360* = 2 * amt/242;
                                       };
                                    publish(@thisConsensusTime/353)
                                      .case("Bob").send({
                                        isClass = False,
                                        msg = [],
                                        pay = [0, (v360, v315 ) ],
                                        when = true})
                                      .recv({
                                        didSend = v199,
                                        from = v361,
                                        msg = [],
                                        secs = v363,
                                        time = v362}){
                                        timeOrder((Just thisConsensusTime/353, thisConsensusTime/362 ), (Just thisConsensusSecs/354, thisConsensusSecs/363 ) );
                                        const v364* = 2 * amt/242;
                                        const v365* = balance(0)/356;
                                        checkPay(0, Nothing);
                                        const v366* = v358;
                                        const v367* = balance(2)/366 + v364;
                                        checkPay(v364, Just v315);
                                        const v368* = v254 == v361;
                                        claim(CT_Require)(v368, Just "sender correct");
                                        const v369* = v367;
                                        const v370* = v367;
                                        const v371* = balance(2)/369 <= balance(2)/370;
                                        claim(CT_Assert)(v371, Just "Token.burn");
                                        const v372* = v367;
                                        const v373* = balance(2)/372 - balance(2)/369;
                                        const v374* = v350;
                                        const v375* = supply(2)/374 - balance(2)/369;
                                        Token(v315).burn(balance(2)/369);
                                        const v376* = false;
                                        const v377* = (destroyed(2)/376 ? false : true);
                                        claim(CT_Assert)(v377, Just "token not yet destroyed at Token.destroy");
                                        const v378* = v375;
                                        const v379* = 0 == supply(2)/378;
                                        claim(CT_Assert)(v379, Just "token supply zero at Token.destroy");
                                        Token(v315).destroy();
                                        const v380* = balance(0)/365;
                                        const v381* = 0 == balance(0)/380;
                                        claim(CT_Assert)(v381, Just "balance zero at application exit");
                                        const v382* = v305;
                                        const v383* = 0 == balance(1)/382;
                                        claim(CT_Assert)(v383, Just "balance zero at application exit");
                                        const v384* = v373;
                                        const v385* = 0 == balance(2)/384;
                                        claim(CT_Assert)(v385, Just "balance zero at application exit");
                                        const v386* = true;
                                        const v387* = (destroyed(1)/386 ? false : true);
                                        claim(CT_Assert)(destroyed(1)/386, Just "token destroyed at application exit");
                                        const v388* = true;
                                        const v389* = (destroyed(2)/388 ? false : true);
                                        claim(CT_Assert)(destroyed(2)/388, Just "token destroyed at application exit");
                                        commit();
                                        exit(); }
                                       }
                                   }
                               }
                           }
                       }
                   }
               }
           }
       }
  