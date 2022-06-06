"reach 0.1";

const Opts = Struct([
  ["token1", Token],
  ["token2", Token],
  ["token3", Token],
  ["token4", Token],
  ["token5", Token],
  ["claimsPoolctcInfo", Contract],
  ["initialDAOWallet", Address],
  ["initialClaimsPool", Address],
]);

const ReturnValues = Struct([
  ["NFTPolicyOwner", Address],
  ["NFTOwnerAmt", UInt],
  ["DAOWalletAmt", UInt],
  ["ClaimsPoolAmt", UInt],
]);

const ReturnValuesAdmin = Struct([
  ["ContractOwner", Address],
  ["NFTOwnerPer", UInt],
  ["DAOWalletPer", UInt],
  ["ClaimsPoolPer", UInt],
  ["ClaimsPoolThreshold", UInt],
]);

const ClaimsReturnValues = Struct([
  ["totalFunds", UInt],
  ["fundsInsured", UInt],
]);

export const main = Reach.App(() => {
  setOptions({
    untrustworthyMaps: false,
    verifyArithmetic: false,
  });

  const Deployer = Participant("Deployer", {
    opts: Opts,
  });
  const User = API("User", {
    createInsuranceCover: Fun([Address, UInt, UInt, UInt, UInt, UInt], ReturnValues), // Create insurance cover
    updateAdminValues: Fun([Address, UInt, UInt, UInt, UInt, UInt, UInt], ReturnValuesAdmin), // remove ASA from the list
    // updatePaymentASA: Fun([Token, UInt], Opts), // add ASA to payment acceptance
  });
  const V = View({
    opts: Opts,
  });
  init();
  Deployer.only(() => {
    const opts = declassify(interact.opts);
    const { token1, token2, token3, token4, token5 } = opts;
    assume(token1 != token2);
    assume(token1 != token3);
    assume(token1 != token4);
    assume(token1 != token5);

    assume(token2 != token1);
    assume(token2 != token3);
    assume(token2 != token4);
    assume(token2 != token5);

    assume(token3 != token2);
    assume(token3 != token1);
    assume(token3 != token4);
    assume(token3 != token5);

    assume(token4 != token2);
    assume(token4 != token3);
    assume(token4 != token1);
    assume(token4 != token5);

    assume(token5 != token2);
    assume(token5 != token3);
    assume(token5 != token4);
    assume(token5 != token1);
  });
  Deployer.publish(opts, token1, token2, token3, token4, token5);
  V.opts.set(opts);
  const {
    initialDAOWallet, initialClaimsPool, claimsPoolctcInfo
  } = opts;
  commit();
  const calculateDistrabution = (per, amt) => {
    return (per * amt) / 100;
  };

  Deployer.publish();

  const C_interface = {
    totalFunds: Fun([], UInt),
    fundsInsured: Fun([], UInt),
    claimsPoolThreshold: Fun([], UInt),
    Invest: Fun([UInt], ClaimsReturnValues)
  };

  const [NFTOwnerPer, DAOWalletPer, ClaimsPoolPer, DAOWallet, ClaimsPool, owner, claimsPoolThreshold,
    token1Disabled, token2Disabled, token3Disabled, token4Disabled, token5Disabled
  ] =
  parallelReduce([
    5,
    15,
    80,
    initialDAOWallet,
    initialClaimsPool,
    Deployer,
    50,
    1,1,0,0,0
    ])
    .define(() => {
    })
    .invariant(balance() == 0 && NFTOwnerPer + DAOWalletPer + ClaimsPoolPer == 100 && balance(token2) >= 0
    && balance(token3) >= 0 && balance(token4) >= 0 && balance(token5) >= 0 && balance(token1) >= 0
    )
    .paySpec([
      token1, token2, token3, token4, token5
    ])
    .while(true)
    .api(
      User.createInsuranceCover,
      (PolicyOwner, token1Amt, token2Amt, token3Amt, token4Amt, token5Amt) => {
        // Complete an assume to check paymentToken is an accepted token
        assume(token1Amt + token2Amt + token3Amt + token4Amt + token5Amt > 0, "You can't send nothing")
      },
      (PolicyOwner, token1Amt, token2Amt, token3Amt, token4Amt, token5Amt) => [0,
        [token1Amt * token1Disabled, token1],
        [token2Amt * token2Disabled, token2],
        [token3Amt * token3Disabled, token3],
        [token4Amt * token4Disabled, token4],
        [token5Amt * token5Disabled, token5],
      ],
      (PolicyOwner, token1Amt, token2Amt, token3Amt, token4Amt, token5Amt, k) => {
        // Check if claims pool + premium amount doesn't go above x% of its total amount
        const claimsPoolContract = remote(claimsPoolctcInfo, C_interface);

        const totalAmt = (token1Amt * token1Disabled) + (token2Amt * token2Disabled) + (
          token3Amt * token3Disabled) + (token4Amt * token4Disabled) + (token5Amt * token5Disabled);

        // const retrunFunctionValue = claimsPoolContract.returnAmt(66);
        // if ((claimsPoolContract.totalFunds() * 100) / ((claimsPoolContract.fundsInsured() + totalAmt) * 100) / 100) > claimsPoolContract.claimsPoolThreshold()

        // if ((((claimsPoolContract.fundsInsured() + totalAmt) / (claimsPoolContract.totalFunds() + 1)) * 100) > claimsPoolContract.claimsPoolThreshold())
        const tmp1 = (claimsPoolContract.fundsInsured() + totalAmt);
        const tmp2 = (claimsPoolContract.totalFunds() + 1);
        const tmpTotal = ((tmp1 / tmp2));
        if (10 > 1)
        {
          // Don't complete the InsuranceCover
          const returnData = ReturnValues.fromObject({
            NFTPolicyOwner: this,
            NFTOwnerAmt: tmpTotal * 100,
            DAOWalletAmt: claimsPoolContract.totalFunds() + 1, //toDAOWallet,
            ClaimsPoolAmt: claimsPoolContract.claimsPoolThreshold(),
          });
  
          k(returnData);
          return [NFTOwnerPer, DAOWalletPer, ClaimsPoolPer, DAOWallet, ClaimsPool, owner, claimsPoolThreshold,
            token1Disabled, token2Disabled, token3Disabled, token4Disabled, token5Disabled];
        } else 
        {
          if (token1Amt * token1Disabled > 0)
          {
          // Complete the transaction
          const toPolicyOwner = calculateDistrabution(NFTOwnerPer, token1Amt * token1Disabled);
          const toDAOWallet = calculateDistrabution(DAOWalletPer, token1Amt * token1Disabled);
          const toClaimsPool = calculateDistrabution(ClaimsPoolPer, token1Amt * token1Disabled);
          // Distribute premiums
            // Policy Polygon owner
            transfer([[toPolicyOwner, token1]]).to(PolicyOwner);
            // DBD DAO Wallet
            transfer([[toDAOWallet, token1]]).to(DAOWallet);
            // Claims pool
            transfer([[toClaimsPool, token1]]).to(ClaimsPool);
          }
          else if (token2Amt * token2Disabled > 0)
          {
          // Complete the transaction
          const toPolicyOwner = calculateDistrabution(NFTOwnerPer, token2Amt * token2Disabled);
          const toDAOWallet = calculateDistrabution(DAOWalletPer, token2Amt * token2Disabled);
          const toClaimsPool = calculateDistrabution(ClaimsPoolPer, token2Amt * token2Disabled);
          // Distribute premiums
            // Policy Polygon owner
            transfer([[toPolicyOwner, token2]]).to(PolicyOwner);
            // DBD DAO Wallet
            transfer([[toDAOWallet, token2]]).to(DAOWallet);
            // Claims pool
            transfer([[toClaimsPool, token2]]).to(ClaimsPool);
          }
          else if (token3Amt * token3Disabled > 0)
          {
          // Complete the transaction
          const toPolicyOwner = calculateDistrabution(NFTOwnerPer, token3Amt * token3Disabled);
          const toDAOWallet = calculateDistrabution(DAOWalletPer, token3Amt * token3Disabled);
          const toClaimsPool = calculateDistrabution(ClaimsPoolPer, token3Amt * token3Disabled);
          // Distribute premiums
            // Policy Polygon owner
            transfer([[toPolicyOwner, token3]]).to(PolicyOwner);
            // DBD DAO Wallet
            transfer([[toDAOWallet, token3]]).to(DAOWallet);
            // Claims pool
            transfer([[toClaimsPool, token3]]).to(ClaimsPool);
          }
          else if (token4Amt * token4Disabled > 0)
          {
          // Complete the transaction
          const toPolicyOwner = calculateDistrabution(NFTOwnerPer, token4Amt * token4Disabled);
          const toDAOWallet = calculateDistrabution(DAOWalletPer, token4Amt * token4Disabled);
          const toClaimsPool = calculateDistrabution(ClaimsPoolPer, token4Amt * token4Disabled);
          // Distribute premiums
            // Policy Polygon owner
            transfer([[toPolicyOwner, token4]]).to(PolicyOwner);
            // DBD DAO Wallet
            transfer([[toDAOWallet, token4]]).to(DAOWallet);
            // Claims pool
            transfer([[toClaimsPool, token4]]).to(ClaimsPool);
          }
          else if (token5Amt * token5Disabled > 0)
          {
          // Complete the transaction
          const toPolicyOwner = calculateDistrabution(NFTOwnerPer, token5Amt * token5Disabled);
          const toDAOWallet = calculateDistrabution(DAOWalletPer, token5Amt * token5Disabled);
          const toClaimsPool = calculateDistrabution(ClaimsPoolPer, token5Amt * token5Disabled);
          // Distribute premiums
            // Policy Polygon owner
            transfer([[toPolicyOwner, token5]]).to(PolicyOwner);
            // DBD DAO Wallet
            transfer([[toDAOWallet, token5]]).to(DAOWallet);
            // Claims pool
            transfer([[toClaimsPool, token5]]).to(ClaimsPool);
          }

          const AmtToPolicyOwner = calculateDistrabution(NFTOwnerPer, totalAmt);
          const AmtToDAO = calculateDistrabution(DAOWalletPer, totalAmt);
          const AmtToClaims = calculateDistrabution(ClaimsPoolPer, totalAmt);

          const returnData = ReturnValues.fromObject({
            NFTPolicyOwner: PolicyOwner,
            NFTOwnerAmt: AmtToPolicyOwner,
            DAOWalletAmt: AmtToDAO,
            ClaimsPoolAmt: AmtToClaims,
          });

        k(returnData);
        return [NFTOwnerPer, DAOWalletPer, ClaimsPoolPer, DAOWallet, ClaimsPool, owner, claimsPoolThreshold,
          token1Disabled, token2Disabled, token3Disabled, token4Disabled, token5Disabled];
        }
      }
    )
    .api(
      User.updateAdminValues,
      (newOwner, newClaimsPoolThreshold,
        newtoken1Disabled, newtoken2Disabled, newtoken3Disabled, newtoken4Disabled, newtoken5Disabled
        ) => {
        assume(this == owner, "You are not the admin of this contract") // check array isn't full
        assume(newtoken1Disabled < 2, "token1 disabled can't be greater than 1")
        assume(newtoken2Disabled < 2, "token2 disabled can't be greater than 1")
        assume(newtoken3Disabled < 2, "token3 disabled can't be greater than 1")
        assume(newtoken4Disabled < 2, "token4 disabled can't be greater than 1")
        assume(newtoken5Disabled < 2, "token5 disabled can't be greater than 1")
      },
      (newOwner, newClaimsPoolThreshold,
        newtoken1Disabled, newtoken2Disabled, newtoken3Disabled, newtoken4Disabled, newtoken5Disabled) => [0, 
        [0, token1],
        [0, token2],
        [0, token3],
        [0, token4],
        [0, token5]],
      (newOwner, newClaimsPoolThreshold,
        newtoken1Disabled, newtoken2Disabled, newtoken3Disabled, newtoken4Disabled, newtoken5Disabled, k) => {

        const returnValues = ReturnValuesAdmin.fromObject({
          ContractOwner: newOwner,
          NFTOwnerPer: NFTOwnerPer,
          DAOWalletPer: DAOWalletPer,
          ClaimsPoolPer: ClaimsPoolPer,
          ClaimsPoolThreshold: newClaimsPoolThreshold,
        });

        k(returnValues);
        return [NFTOwnerPer, DAOWalletPer, ClaimsPoolPer, DAOWallet, ClaimsPool, newOwner, newClaimsPoolThreshold,
          newtoken1Disabled, newtoken2Disabled, newtoken3Disabled, newtoken4Disabled, newtoken5Disabled];
      }
    );
  commit();
  fork().case(
    Deployer,
    () => ({}),
    (_) => 0,
    () => {}
  );

  transfer([[balance(token1), token1]]).to(Deployer);
  transfer([[balance(token2), token2]]).to(Deployer);
  transfer([[balance(token3), token3]]).to(Deployer);
  transfer([[balance(token4), token4]]).to(Deployer);
  transfer([[balance(token5), token5]]).to(Deployer);
  transfer(balance()).to(Deployer);
  commit();

  exit();
});
