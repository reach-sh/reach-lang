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
only(Left "Alice") {
  const v225* = protect<Object({"amt": UInt, "metadata": Bytes(32), "name": Bytes(32), "supply": UInt, "symbol": Bytes(8), "url": Bytes(96)})>("Alice".interact.getParams());
  const v226! = v225.name;
  const v227! = v225.symbol;
  const v228! = v225.url;
  const v229! = v225.metadata;
  const v230* = v225.supply;
  const v231* = v225.amt;
  const v232* = 4 * v231;
  const v233! = v232 <= v230;
  claim(CT_Assume False)(v233, Nothing);
  const v235! = v232 <= UInt.max;
  claim(CT_Assume False)(v235, Nothing);
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
    checkPay(0, Nothing);
    const v246* = 4 * amt/242;
    const v247! = v246 <= supply/241;
    claim(CT_Require)(v247, Nothing);
    const v249! = v246 <= UInt.max;
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
      const v253! = {
        metadata = metadata/240,
        name = name/237,
        supply = supply/241,
        symbol = symbol/238,
        url = url/239};
      protect<Null>("Alice".interact.showToken(v251, v253 ));
       };
    commit();
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
        checkPay(0, Nothing);
        only(Left "Bob") {
          const v259! = {
            metadata = metadata/240,
            name = name/237,
            supply = supply/241,
            symbol = symbol/238,
            url = url/239};
          protect<Null>("Bob".interact.showToken(v251, v259 ));
           };
        commit();
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
            checkPay(0, Nothing);
            const v264! = v254 == v260;
            claim(CT_Require)(v264, Just "sender correct");
            const v265* = 2 * amt/242;
            const v267! = v265 <= supply/241;
            claim(CT_Assert)(v267, Just "balance sufficient for transfer");
            const v269* = supply/241 - v265;
            transfer.(v265, Just v251).to(v254);
            only(Left "Bob") {
              protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
               };
            commit();
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
                checkPay(0, Nothing);
                const v276! = v236 == v272;
                claim(CT_Require)(v276, Just "sender correct");
                const v279! = v265 <= v269;
                claim(CT_Assert)(v279, Just "balance sufficient for transfer");
                const v281! = v269 - v265;
                transfer.(v265, Just v251).to(v236);
                only(Left "Alice") {
                  protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
                   };
                commit();
                publish(@thisConsensusTime/273)
                  .case("Alice").send({
                    isClass = False,
                    msg = [],
                    pay = [0, (v265, v251 ) ],
                    when = true})
                  .recv({
                    didSend = v106,
                    from = v285,
                    msg = [],
                    secs = v287,
                    time = v286}){
                    timeOrder((Just thisConsensusTime/273, thisConsensusTime/286 ), (Just thisConsensusSecs/274, thisConsensusSecs/287 ) );
                    checkPay(0, Nothing);
                    const v291! = v281 + v265;
                    checkPay(v265, Just v251);
                    const v292! = v236 == v285;
                    claim(CT_Require)(v292, Just "sender correct");
                    commit();
                    publish(@thisConsensusTime/286)
                      .case("Bob").send({
                        isClass = False,
                        msg = [],
                        pay = [0, (v265, v251 ) ],
                        when = true})
                      .recv({
                        didSend = v116,
                        from = v294,
                        msg = [],
                        secs = v296,
                        time = v295}){
                        timeOrder((Just thisConsensusTime/286, thisConsensusTime/295 ), (Just thisConsensusSecs/287, thisConsensusSecs/296 ) );
                        checkPay(0, Nothing);
                        const v300* = v291 + v265;
                        checkPay(v265, Just v251);
                        const v301! = v254 == v294;
                        claim(CT_Require)(v301, Just "sender correct");
                        const v303! = supply/241 <= v300;
                        claim(CT_Assert)(v303, Just "Token.burn");
                        const v305! = v300 - supply/241;
                        const v307! = supply/241 - supply/241;
                        Token(v251).burn(supply/241);
                        const v311! = 0 == v307;
                        claim(CT_Assert)(v311, Just "token supply zero at Token.destroy");
                        Token(v251).destroy();
                        const v312! = "                                                                                                ";
                        const v313! = "                                ";
                        const v314* = new Token({
                          decimals = null,
                          metadata = v313,
                          name = name/237,
                          supply = UInt.max,
                          sym = symbol/238,
                          url = v312});
                        const v315* = emitLog(internal)(v314 );
                        only(Left "Alice") {
                          const v317! = {
                            name = name/237,
                            symbol = symbol/238};
                          protect<Null>("Alice".interact.showToken(v315, v317 ));
                           };
                        only(Left "Bob") {
                          const v319! = {
                            name = name/237,
                            symbol = symbol/238};
                          protect<Null>("Bob".interact.showToken(v315, v319 ));
                           };
                        commit();
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
                            checkPay(0, Nothing);
                            const v324! = v254 == v320;
                            claim(CT_Require)(v324, Just "sender correct");
                            const v327! = v265 <= UInt.max;
                            claim(CT_Assert)(v327, Just "balance sufficient for transfer");
                            const v329* = UInt.max - v265;
                            transfer.(v265, Just v315).to(v254);
                            only(Left "Bob") {
                              protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
                               };
                            commit();
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
                                checkPay(0, Nothing);
                                const v336! = v236 == v332;
                                claim(CT_Require)(v336, Just "sender correct");
                                const v339! = v265 <= v329;
                                claim(CT_Assert)(v339, Just "balance sufficient for transfer");
                                const v341* = v329 - v265;
                                transfer.(v265, Just v315).to(v236);
                                only(Left "Alice") {
                                  protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
                                   };
                                const v346! = v341 <= v341;
                                claim(CT_Assert)(v346, Just "Token.burn");
                                const v348! = v341 - v341;
                                const v350! = UInt.max - v341;
                                Token(v315).burn(v341);
                                commit();
                                publish(@thisConsensusTime/333)
                                  .case("Alice").send({
                                    isClass = False,
                                    msg = [],
                                    pay = [0, (v265, v315 ) ],
                                    when = true})
                                  .recv({
                                    didSend = v189,
                                    from = v352,
                                    msg = [],
                                    secs = v354,
                                    time = v353}){
                                    timeOrder((Just thisConsensusTime/333, thisConsensusTime/353 ), (Just thisConsensusSecs/334, thisConsensusSecs/354 ) );
                                    checkPay(0, Nothing);
                                    const v358! = v348 + v265;
                                    checkPay(v265, Just v315);
                                    const v359! = v236 == v352;
                                    claim(CT_Require)(v359, Just "sender correct");
                                    commit();
                                    publish(@thisConsensusTime/353)
                                      .case("Bob").send({
                                        isClass = False,
                                        msg = [],
                                        pay = [0, (v265, v315 ) ],
                                        when = true})
                                      .recv({
                                        didSend = v199,
                                        from = v361,
                                        msg = [],
                                        secs = v363,
                                        time = v362}){
                                        timeOrder((Just thisConsensusTime/353, thisConsensusTime/362 ), (Just thisConsensusSecs/354, thisConsensusSecs/363 ) );
                                        checkPay(0, Nothing);
                                        const v367* = v358 + v265;
                                        checkPay(v265, Just v315);
                                        const v368! = v254 == v361;
                                        claim(CT_Require)(v368, Just "sender correct");
                                        const v371! = v367 <= v367;
                                        claim(CT_Assert)(v371, Just "Token.burn");
                                        const v373! = v367 - v367;
                                        const v375! = v350 - v367;
                                        Token(v315).burn(v367);
                                        const v379! = 0 == v375;
                                        claim(CT_Assert)(v379, Just "token supply zero at Token.destroy");
                                        Token(v315).destroy();
                                        const v383! = 0 == v305;
                                        claim(CT_Assert)(v383, Just "balance zero at application exit");
                                        const v385! = 0 == v373;
                                        claim(CT_Assert)(v385, Just "balance zero at application exit");
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
  