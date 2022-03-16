import {LocalStep, Locals, store, storePayload, apiB, apiSet} from '../types'
export default async function renderObjects  ({nodeId, c }:{nodeId: string, c: any }){
  const r = await c.getStateLocals(nodeId)
  const apis = await c.getAPIs()
  
  type ActorSet = {
    [index: string]: any
  }

  function hasKey<O>(obj: O, key: PropertyKey): key is keyof O {
    return key in obj
    }

  let actorSet = {}
  for (const [key, value] of Object.entries(r.l_locals)) {
      const value2 = value as LocalStep
      const who = value2.l_who ? value2.l_who : 'Consensus';
      if(hasKey(actorSet, key)){
        actorSet[key] = who as never
        }
    }
    
  let apiSet = {}
  for (const [key,value] of Object.entries(apis)) {
    const value2 = value as apiB
    const who = value2.a_name
    if(hasKey(apiSet, key)){
        apiSet[key] = who as never
        }
  }
    
    let actors = []
    let actorsNoCons = []
    const actorEntries = Object.entries(actorSet)
    // NOTE: assumption: there is at least one non-consensus actor
    const firstActorId = actorEntries[0][0]
    for (const [key,value] of actorEntries) {
        actors.push(`<option value="${key}">${value}</option>`) 
    }
    for (const [key,value] of actorEntries) {
        if (parseInt(key) !== -1 ) {
            actorsNoCons.push(`<option value="${key}">${value}</option>`)
        }
    }
    
    let obs = []
    for (const [key,value] of Object.entries(r.l_locals)) {
        const value2 = value as LocalStep
        const who = value2.l_who ? value2.l_who : 'Consensus'
        let status = 'Initial'
        switch (value2.l_ks) {
            case 'PS_Suspend':
                status = 'Program Running'
                break;
                case 'PS_Done':
                    status = 'Program Done'
                    break;
                }
                obs.push({
                    actorId: `${key}`,
                    nodeId:`${nodeId}`,
                    actorSet:`${JSON.stringify(actorSet)}`,
                    apiSet:`${JSON.stringify(apiSet)}`,
                    who: `${who}`
                })
            }

        let apiBs = []
            for (const [key,value] of Object.entries(apis)) {
                let status = 'Initial'
                const value2 = value as apiSet
                let f = value2.a_liv.out.contents[0][1].contents
                apiBs.push({
                    nodeId: `${nodeId}`,
                    apiId: `${key}`,
                    apiName: `${value2.a_name}`,
                    fn: `${JSON.stringify(f)}`,
                    name: `${value2.a_name}`
                })
  }

  return { obs, actorsNoCons, actors, apiBs }
  
}