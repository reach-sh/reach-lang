'reach 0.1'

// APIs and Views of up to 12 args are supported.
const manyArgFunSig = Fun([UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                          ],
                          UInt)

const sum = (v0,v1,v2,v3
             , v4,v5,v6,v7
             , v8,v9,v10,v11
            ) => {
              return v0 + v1 + v2 + v3
                + v4 + v5 + v6 + v7
                + v8 + v9 + v10 + v11
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
      ETH,
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

