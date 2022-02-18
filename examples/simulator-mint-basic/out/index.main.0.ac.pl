#lang pl
{
  }
// maps
{
  }
// initialization

APIs:{
  }
{
  "Alice" = interact {
    didTransfer = IT_Fun [Bool,UInt] Null,
    getParams = IT_Fun [] Object({"amt": UInt, "metadata": Bytes(32), "name": Bytes(32), "supply": UInt, "symbol": Bytes(8), "url": Bytes(96)}),
    showToken = IT_UDFun Null};
  only(Right False) {
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
  send({
    amt = [0, ],
    as = (v226, v227, v228, v229, v230, v231 ),
    saved = (),
    soloSend = True,
    when = true,
    which = 0})
  recv({
    didSendv = v43,
    from = v236,
    lct = Just 0,
    msg = (v237, v238, v239, v240, v241, v242 ),
    out = (),
    prev = 0,
    secsv = v244,
    timev = v243,
    which = 0})
  {
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
    only(Right True) {
      const v253! = {
        metadata = metadata/240,
        name = name/237,
        supply = supply/241,
        symbol = symbol/238,
        url = url/239};
      protect<Null>("Alice".interact.showToken(v251, v253 ));
       };
    fromConsensus 1 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251)]) ;
    recv({
      didSendv = v59,
      from = v254,
      lct = Just thisConsensusTime/243,
      msg = (),
      out = (),
      prev = 1,
      secsv = v256,
      timev = v255,
      which = 1})
    {
      checkPay(0, Nothing);
      fromConsensus 2 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251), (v254, v254)]) ;
      recv({
        didSendv = v70,
        from = v260,
        lct = Just thisConsensusTime/255,
        msg = (),
        out = (),
        prev = 2,
        secsv = v262,
        timev = v261,
        which = 2})
      {
        checkPay(0, Nothing);
        const v264! = v254 == v260;
        claim(CT_Require)(v264, Just "sender correct");
        const v265* = 2 * amt/242;
        transfer.(v265, Just v251).to(v254);
        fromConsensus 3 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
        send({
          amt = [0, ],
          as = (),
          saved = (v236, v237, v238, v241, v251, v254, v265 ),
          soloSend = True,
          when = true,
          which = 3})
        recv({
          didSendv = v86,
          from = v272,
          lct = Just thisConsensusTime/261,
          msg = (),
          out = (),
          prev = 3,
          secsv = v274,
          timev = v273,
          which = 3})
        {
          checkPay(0, Nothing);
          const v276! = v236 == v272;
          claim(CT_Require)(v276, Just "sender correct");
          transfer.(v265, Just v251).to(v236);
          only(Right True) {
            protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
             };
          fromConsensus 4 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
          send({
            amt = [0, (v265, v251 ) ],
            as = (),
            saved = (v236, v237, v238, v241, v251, v254, v265 ),
            soloSend = True,
            when = true,
            which = 4})
          recv({
            didSendv = v106,
            from = v285,
            lct = Just thisConsensusTime/273,
            msg = (),
            out = (),
            prev = 4,
            secsv = v287,
            timev = v286,
            which = 4})
          {
            checkPay(0, Nothing);
            checkPay(v265, Just v251);
            const v292! = v236 == v285;
            claim(CT_Require)(v292, Just "sender correct");
            fromConsensus 5 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
            recv({
              didSendv = v116,
              from = v294,
              lct = Just thisConsensusTime/286,
              msg = (),
              out = (),
              prev = 5,
              secsv = v296,
              timev = v295,
              which = 5})
            {
              checkPay(0, Nothing);
              checkPay(v265, Just v251);
              const v301! = v254 == v294;
              claim(CT_Require)(v301, Just "sender correct");
              Token(v251).burn(supply/241);
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
              only(Right True) {
                const v317! = {
                  name = name/237,
                  symbol = symbol/238};
                protect<Null>("Alice".interact.showToken(v315, v317 ));
                 };
              fromConsensus 6 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315)]) ;
              recv({
                didSendv = v146,
                from = v320,
                lct = Just thisConsensusTime/295,
                msg = (),
                out = (),
                prev = 6,
                secsv = v322,
                timev = v321,
                which = 6})
              {
                checkPay(0, Nothing);
                const v324! = v254 == v320;
                claim(CT_Require)(v324, Just "sender correct");
                const v329* = UInt.max - v265;
                transfer.(v265, Just v315).to(v254);
                fromConsensus 7 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v329, v329)]) ;
                send({
                  amt = [0, ],
                  as = (),
                  saved = (v236, v254, v265, v315, v329 ),
                  soloSend = True,
                  when = true,
                  which = 7})
                recv({
                  didSendv = v162,
                  from = v332,
                  lct = Just thisConsensusTime/321,
                  msg = (),
                  out = (),
                  prev = 7,
                  secsv = v334,
                  timev = v333,
                  which = 7})
                {
                  checkPay(0, Nothing);
                  const v336! = v236 == v332;
                  claim(CT_Require)(v336, Just "sender correct");
                  const v341* = v329 - v265;
                  transfer.(v265, Just v315).to(v236);
                  only(Right True) {
                    protect<Null>("Alice".interact.didTransfer(true, amt/242 ));
                     };
                  const v348* = v341 - v341;
                  Token(v315).burn(v341);
                  fromConsensus 8 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v348, v348)]) ;
                  send({
                    amt = [0, (v265, v315 ) ],
                    as = (),
                    saved = (v236, v254, v265, v315, v348 ),
                    soloSend = True,
                    when = true,
                    which = 8})
                  recv({
                    didSendv = v189,
                    from = v352,
                    lct = Just thisConsensusTime/333,
                    msg = (),
                    out = (),
                    prev = 8,
                    secsv = v354,
                    timev = v353,
                    which = 8})
                  {
                    checkPay(0, Nothing);
                    const v358* = v348 + v265;
                    checkPay(v265, Just v315);
                    const v359! = v236 == v352;
                    claim(CT_Require)(v359, Just "sender correct");
                    fromConsensus 9 (continue [(v254, v254), (v265, v265), (v315, v315), (v358, v358)]) ;
                    recv({
                      didSendv = v199,
                      from = v361,
                      lct = Just thisConsensusTime/353,
                      msg = (),
                      out = (),
                      prev = 9,
                      secsv = v363,
                      timev = v362,
                      which = 9})
                    {
                      checkPay(0, Nothing);
                      const v367! = v358 + v265;
                      checkPay(v265, Just v315);
                      const v368! = v254 == v361;
                      claim(CT_Require)(v368, Just "sender correct");
                      Token(v315).burn(v367);
                      Token(v315).destroy();
                      fromConsensus 10 (halt []) ;
                       } } } } } } } } } },
  "Bob" = interact {
    didTransfer = IT_Fun [Bool,UInt] Null,
    showToken = IT_UDFun Null};
  recv({
    didSendv = v43,
    from = v236,
    lct = Just 0,
    msg = (v237, v238, v239, v240, v241, v242 ),
    out = (),
    prev = 0,
    secsv = v244,
    timev = v243,
    which = 0})
  {
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
    fromConsensus 1 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251)]) ;
    send({
      amt = [0, ],
      as = (),
      saved = (v236, v237, v238, v241, v242, v251 ),
      soloSend = True,
      when = true,
      which = 1})
    recv({
      didSendv = v59,
      from = v254,
      lct = Just thisConsensusTime/243,
      msg = (),
      out = (),
      prev = 1,
      secsv = v256,
      timev = v255,
      which = 1})
    {
      checkPay(0, Nothing);
      only(Right True) {
        const v259! = {
          metadata = metadata/240,
          name = name/237,
          supply = supply/241,
          symbol = symbol/238,
          url = url/239};
        protect<Null>("Bob".interact.showToken(v251, v259 ));
         };
      fromConsensus 2 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251), (v254, v254)]) ;
      send({
        amt = [0, ],
        as = (),
        saved = (v236, v237, v238, v241, v242, v251, v254 ),
        soloSend = True,
        when = true,
        which = 2})
      recv({
        didSendv = v70,
        from = v260,
        lct = Just thisConsensusTime/255,
        msg = (),
        out = (),
        prev = 2,
        secsv = v262,
        timev = v261,
        which = 2})
      {
        checkPay(0, Nothing);
        const v264! = v254 == v260;
        claim(CT_Require)(v264, Just "sender correct");
        const v265* = 2 * amt/242;
        transfer.(v265, Just v251).to(v254);
        only(Right True) {
          protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
           };
        fromConsensus 3 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
        recv({
          didSendv = v86,
          from = v272,
          lct = Just thisConsensusTime/261,
          msg = (),
          out = (),
          prev = 3,
          secsv = v274,
          timev = v273,
          which = 3})
        {
          checkPay(0, Nothing);
          const v276! = v236 == v272;
          claim(CT_Require)(v276, Just "sender correct");
          transfer.(v265, Just v251).to(v236);
          fromConsensus 4 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
          recv({
            didSendv = v106,
            from = v285,
            lct = Just thisConsensusTime/273,
            msg = (),
            out = (),
            prev = 4,
            secsv = v287,
            timev = v286,
            which = 4})
          {
            checkPay(0, Nothing);
            checkPay(v265, Just v251);
            const v292! = v236 == v285;
            claim(CT_Require)(v292, Just "sender correct");
            fromConsensus 5 (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)]) ;
            send({
              amt = [0, (v265, v251 ) ],
              as = (),
              saved = (v236, v237, v238, v241, v251, v254, v265 ),
              soloSend = True,
              when = true,
              which = 5})
            recv({
              didSendv = v116,
              from = v294,
              lct = Just thisConsensusTime/286,
              msg = (),
              out = (),
              prev = 5,
              secsv = v296,
              timev = v295,
              which = 5})
            {
              checkPay(0, Nothing);
              checkPay(v265, Just v251);
              const v301! = v254 == v294;
              claim(CT_Require)(v301, Just "sender correct");
              Token(v251).burn(supply/241);
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
              only(Right True) {
                const v319! = {
                  name = name/237,
                  symbol = symbol/238};
                protect<Null>("Bob".interact.showToken(v315, v319 ));
                 };
              fromConsensus 6 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315)]) ;
              send({
                amt = [0, ],
                as = (),
                saved = (v236, v254, v265, v315 ),
                soloSend = True,
                when = true,
                which = 6})
              recv({
                didSendv = v146,
                from = v320,
                lct = Just thisConsensusTime/295,
                msg = (),
                out = (),
                prev = 6,
                secsv = v322,
                timev = v321,
                which = 6})
              {
                checkPay(0, Nothing);
                const v324! = v254 == v320;
                claim(CT_Require)(v324, Just "sender correct");
                const v329* = UInt.max - v265;
                transfer.(v265, Just v315).to(v254);
                only(Right True) {
                  protect<Null>("Bob".interact.didTransfer(true, amt/242 ));
                   };
                fromConsensus 7 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v329, v329)]) ;
                recv({
                  didSendv = v162,
                  from = v332,
                  lct = Just thisConsensusTime/321,
                  msg = (),
                  out = (),
                  prev = 7,
                  secsv = v334,
                  timev = v333,
                  which = 7})
                {
                  checkPay(0, Nothing);
                  const v336! = v236 == v332;
                  claim(CT_Require)(v336, Just "sender correct");
                  const v341* = v329 - v265;
                  transfer.(v265, Just v315).to(v236);
                  const v348* = v341 - v341;
                  Token(v315).burn(v341);
                  fromConsensus 8 (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v348, v348)]) ;
                  recv({
                    didSendv = v189,
                    from = v352,
                    lct = Just thisConsensusTime/333,
                    msg = (),
                    out = (),
                    prev = 8,
                    secsv = v354,
                    timev = v353,
                    which = 8})
                  {
                    checkPay(0, Nothing);
                    const v358* = v348 + v265;
                    checkPay(v265, Just v315);
                    const v359! = v236 == v352;
                    claim(CT_Require)(v359, Just "sender correct");
                    fromConsensus 9 (continue [(v254, v254), (v265, v265), (v315, v315), (v358, v358)]) ;
                    send({
                      amt = [0, (v265, v315 ) ],
                      as = (),
                      saved = (v254, v265, v315, v358 ),
                      soloSend = True,
                      when = true,
                      which = 9})
                    recv({
                      didSendv = v199,
                      from = v361,
                      lct = Just thisConsensusTime/353,
                      msg = (),
                      out = (),
                      prev = 9,
                      secsv = v363,
                      timev = v362,
                      which = 9})
                    {
                      checkPay(0, Nothing);
                      const v367! = v358 + v265;
                      checkPay(v265, Just v315);
                      const v368! = v254 == v361;
                      claim(CT_Require)(v368, Just "sender correct");
                      Token(v315).burn(v367);
                      Token(v315).destroy();
                      fromConsensus 10 (halt []) ;
                       } } } } } } } } } }}


views: ({
  }, {
  1 = (view [v236, v237, v238, v241, v242, v251] {
    }),
  2 = (view [v236, v237, v238, v241, v242, v251, v254] {
    }),
  3 = (view [v236, v237, v238, v241, v251, v254, v265] {
    }),
  4 = (view [v236, v237, v238, v241, v251, v254, v265] {
    }),
  5 = (view [v236, v237, v238, v241, v251, v254, v265] {
    }),
  6 = (view [v236, v254, v265, v315] {
    }),
  7 = (view [v236, v254, v265, v315, v329] {
    }),
  8 = (view [v236, v254, v265, v315, v348] {
    }),
  9 = (view [v254, v265, v315, v358] {
    })})
apiInfo: {
  }
events: {
  }
{
  0 = {
    v236,
    (between [Nothing] [Nothing]),
    last = 0,
    [],
    [],
    [v237*, v238*, v239!, v240!, v241*, v242*],
    [Bytes(32), Bytes(8), Bytes(96), Bytes(32), UInt, UInt],
    timev = v243,
    secsv = v244,
    {
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
      (from 1, (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251)])) }
     },
  1 = {
    v254,
    (between [Nothing] [Nothing]),
    last = 1,
    [v236*, v237*, v238*, v241*, v242*, v251*],
    [Address, Bytes(32), Bytes(8), UInt, UInt, Token],
    [],
    [],
    timev = v255,
    secsv = v256,
    {
      checkPay(0, Nothing);
      (from 2, (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v242, amt/242), (v251, v251), (v254, v254)])) }
     },
  2 = {
    v260,
    (between [Nothing] [Nothing]),
    last = 2,
    [v236*, v237*, v238*, v241*, v242!, v251*, v254*],
    [Address, Bytes(32), Bytes(8), UInt, UInt, Token, Address],
    [],
    [],
    timev = v261,
    secsv = v262,
    {
      checkPay(0, Nothing);
      const v264! = v254 == v260;
      claim(CT_Require)(v264, Just "sender correct");
      const v265* = 2 * amt/242;
      transfer.(v265, Just v251).to(v254);
      (from 3, (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)])) }
     },
  3 = {
    v272,
    (between [Nothing] [Nothing]),
    last = 3,
    [v236*, v237*, v238*, v241*, v251*, v254*, v265*],
    [Address, Bytes(32), Bytes(8), UInt, Token, Address, UInt],
    [],
    [],
    timev = v273,
    secsv = v274,
    {
      checkPay(0, Nothing);
      const v276! = v236 == v272;
      claim(CT_Require)(v276, Just "sender correct");
      transfer.(v265, Just v251).to(v236);
      (from 4, (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)])) }
     },
  4 = {
    v285,
    (between [Nothing] [Nothing]),
    last = 4,
    [v236*, v237*, v238*, v241*, v251*, v254*, v265*],
    [Address, Bytes(32), Bytes(8), UInt, Token, Address, UInt],
    [],
    [],
    timev = v286,
    secsv = v287,
    {
      checkPay(0, Nothing);
      checkPay(v265, Just v251);
      const v292! = v236 == v285;
      claim(CT_Require)(v292, Just "sender correct");
      (from 5, (continue [(v236, v236), (v237, name/237), (v238, symbol/238), (v241, supply/241), (v251, v251), (v254, v254), (v265, v265)])) }
     },
  5 = {
    v294,
    (between [Nothing] [Nothing]),
    last = 5,
    [v236*, v237!, v238!, v241!, v251*, v254*, v265*],
    [Address, Bytes(32), Bytes(8), UInt, Token, Address, UInt],
    [],
    [],
    timev = v295,
    secsv = v296,
    {
      checkPay(0, Nothing);
      checkPay(v265, Just v251);
      const v301! = v254 == v294;
      claim(CT_Require)(v301, Just "sender correct");
      Token(v251).burn(supply/241);
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
      (from 6, (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315)])) }
     },
  6 = {
    v320,
    (between [Nothing] [Nothing]),
    last = 6,
    [v236*, v254*, v265*, v315*],
    [Address, Address, UInt, Token],
    [],
    [],
    timev = v321,
    secsv = v322,
    {
      checkPay(0, Nothing);
      const v324! = v254 == v320;
      claim(CT_Require)(v324, Just "sender correct");
      const v329* = UInt.max - v265;
      transfer.(v265, Just v315).to(v254);
      (from 7, (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v329, v329)])) }
     },
  7 = {
    v332,
    (between [Nothing] [Nothing]),
    last = 7,
    [v236*, v254*, v265*, v315*, v329!],
    [Address, Address, UInt, Token, UInt],
    [],
    [],
    timev = v333,
    secsv = v334,
    {
      checkPay(0, Nothing);
      const v336! = v236 == v332;
      claim(CT_Require)(v336, Just "sender correct");
      const v341* = v329 - v265;
      transfer.(v265, Just v315).to(v236);
      const v348* = v341 - v341;
      Token(v315).burn(v341);
      (from 8, (continue [(v236, v236), (v254, v254), (v265, v265), (v315, v315), (v348, v348)])) }
     },
  8 = {
    v352,
    (between [Nothing] [Nothing]),
    last = 8,
    [v236!, v254*, v265*, v315*, v348!],
    [Address, Address, UInt, Token, UInt],
    [],
    [],
    timev = v353,
    secsv = v354,
    {
      checkPay(0, Nothing);
      const v358* = v348 + v265;
      checkPay(v265, Just v315);
      const v359! = v236 == v352;
      claim(CT_Require)(v359, Just "sender correct");
      (from 9, (continue [(v254, v254), (v265, v265), (v315, v315), (v358, v358)])) }
     },
  9 = {
    v361,
    (between [Nothing] [Nothing]),
    last = 9,
    [v254!, v265*, v315*, v358!],
    [Address, UInt, Token, UInt],
    [],
    [],
    timev = v362,
    secsv = v363,
    {
      checkPay(0, Nothing);
      const v367! = v358 + v265;
      checkPay(v265, Just v315);
      const v368! = v254 == v361;
      claim(CT_Require)(v368, Just "sender correct");
      Token(v315).burn(v367);
      Token(v315).destroy();
      (from 10, (halt [])) }
     }}