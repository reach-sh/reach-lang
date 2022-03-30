export const redraw = async (c: any) => {
  let edges = await c.getEdges()
  let states = await c.getStates()
  let elements = []
  const singleColors = ['#0f4539','#2f3b22','#052d0a','#001f24']
  for (const [s, dets] of Object.entries(states)) {
    const details = dets as any
    let displayLabel = details[1].tag.slice(2)
    let actorStateID = details[0]
    if (displayLabel !== 'None') {
      displayLabel = displayLabel + '?'
    }
    elements.push(
      {
        data:
          { id: s,
            label: displayLabel,
            color: singleColors[parseInt(actorStateID)+1]
          }
      }
    )
  }
  for (const [index, value] of edges.entries()) {
    const from = value[0]
    const to = value[1]
    elements.push({data:
      { id: `edge-${index}`, source: from, target: to }
    })
  }
  // citation: animation adapted from https://gist.github.com/maxkfranz/aedff159b0df05ccfaa5
  // and https://stackoverflow.com/questions/40096407/animate-building-a-graph-in-cytoscape-js
  return elements
}