export default async function   ({nodeId, c }: {nodeId: string, c: any }){
  const r = await c.getStateLocals(nodeId)
  const apis = await c.getAPIs()
  const views = await c.getViews(nodeId)
  
  return { nodeId, locals: r, apis, views } 
}