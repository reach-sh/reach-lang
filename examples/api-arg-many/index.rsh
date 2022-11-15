'reach 0.1'

const manyArgFunSig = Fun([UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                          ],
                          UInt)

const sum = (v0,v1,v2,v3
             , v4,v5,v6,v7
             , v8,v9,v10,v11
             , v12,v13,v14,v15
             , v16,v17,v18,v19
            ) => {
              return v0 + v1 + v2 + v3
                + v4 + v5 + v6 + v7
                + v8 + v9 + v10 + v11
                + v12 + v13 + v14 + v15
                + v16 + v17 + v18 + v19
            }

export const mainServer = Reach.App(() => {
  const Deployer = Participant('Deployer', {
    ready: Fun([], Null),
  })
  const A = API({
    sumMany: manyArgFunSig,
  })
  const V = View({
    vSumMany: manyArgFunSig,
  })
  setOptions({
    connectors: [
      // ETH is limited to 12 arguments for APIs or Views before the Solidity compiler complains about stack depth.
      //ETH,
      ALGO,
    ]
  })
  init()

  Deployer.publish()
  V.vSumMany.set(sum)
  Deployer.interact.ready()
  commit()

  const [args, sumManyRet] = call(A.sumMany)
  sumManyRet(sum(...args))
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
  const sumRet = sumServer.sumMany(...args)
  pokeRet(sumRet)
  commit()
})
