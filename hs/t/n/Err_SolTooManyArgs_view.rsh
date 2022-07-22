'reach 0.1'

// APIs and Views of 13+ args are unsupported due to solc stack depth issues.
const manyArgFunSig = Fun([UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt, UInt, UInt, UInt,
                           UInt,
                          ],
                          UInt)

const sum = (v0,v1,v2,v3
             , v4,v5,v6,v7
             , v8,v9,v10,v11
             , v12
            ) => {
              return v0 + v1 + v2 + v3
                + v4 + v5 + v6 + v7
                + v8 + v9 + v10 + v11
                + v12
            }

export const mainServer = Reach.App(() => {
  const Deployer = Participant('Deployer', {
    ready: Fun([], Null),
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

  // Needs a last call to delete the program.
  Deployer.publish()
  commit()

})

