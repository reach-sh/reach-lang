'reach 0.1'

const manyArgFunSig = Fun([UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           //UInt, UInt, UInt, UInt,
                           //UInt, UInt, UInt, UInt,
                           //// I wanted to test more than 15, but when 16 worked I
                           //// decided to keep going higher.
                           //UInt, UInt, UInt, UInt,
                          ],
                          UInt)

export const mainServer = Reach.App(() => {
  const Deployer = Participant('Deployer', {
    ready: Fun([], Null),
  })
  const A = API({
    sumMany: manyArgFunSig,
  })
  setOptions({
    connectors: [
      // With 16 arguments, the Solidity compiler errors saying that the stack is too deep.
      //ETH,
      ALGO,
    ]
  })
  init()

  Deployer.publish()
  Deployer.interact.ready()
  commit()

  const [args, sumManyRet] = call(A.sumMany)

  // FIXME - Reach doesn't yet support turning a DL Tuple into an array.
  //const asArray = array(UInt, args)
  const asArray = array(UInt, [args[0], args[1], args[2], args[3],
                               args[4], args[5], args[6], args[7],
                               //args[8], args[9], args[10], args[11],
                               //args[12], args[13], args[14], args[15],
                               //args[16], args[17], args[18], args[19],
                              ])
  const sum = asArray.reduce(0, (accum, x) => accum + x)
  sumManyRet(sum)

  commit()

  // Needs a last call to delete the program.
  Deployer.publish()
  commit()

})

export const mainClient = Reach.App(() => {
  const Deployer = Participant('Deployer', {
    serverCtcInfo: Contract,
    ready: Fun([], Null),
  })
  const A = API({
    poke: manyArgFunSig,
  })
  setOptions({
    connectors: [
      // With 16 arguments, the Solidity compiler errors saying that the stack is too deep.
      //ETH,
      ALGO,
    ]
  })
  init()

  Deployer.only(() => {
    const serverCtcInfo = declassify(interact.serverCtcInfo)
  })

  Deployer.publish(serverCtcInfo)
  Deployer.interact.ready()

  const sumServer = remote(serverCtcInfo, {
    sumMany: manyArgFunSig,
  })

  commit()

  const [args, pokeRet] = call(A.poke)

  const sum = sumServer.sumMany(...args)

  pokeRet(sum)

  commit()

})
