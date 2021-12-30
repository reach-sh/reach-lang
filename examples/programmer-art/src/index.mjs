import * as c from '@reach-sh/simulator-client';
import cytoscape from 'cytoscape';
import klay from 'cytoscape-klay';

cytoscape.use( klay );

const redraw = async () => {
  let edges = await c.getEdges()
  let states = await c.getStates()
  let elements = []
  for (const s of states) {
    elements.push({data: {id: s}})
  }
  for (const [index, value] of edges.entries()) {
    const from = value[0]
    const to = value[1]
    elements.push({data: { id: `edge-${index}`, source: from, target: to } })
  }
  let cy = cytoscape({
    container: document.getElementById('cy'),
    elements: elements,
    style: [
      {
        selector: 'node',
        style: {
          'background-color': '#666',
          'label': 'data(id)'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 3,
          'line-color': '#ccc',
          'target-arrow-color': '#ccc',
          'target-arrow-shape': 'triangle',
          'curve-style': 'bezier'
        }
      }
    ],
    layout: {
      name: 'klay'
    }
  });
}

const log = document.querySelector("#output")

const localsBtn = document.querySelector("#localsButton")
const locals = async () => {
  let n = parseInt(document.querySelector("#stateLocalsFor").value)
  let r = await c.getStateLocals(n)
  appendToLog(JSON.stringify(r,null,2))
}
localsBtn.addEventListener("click",locals)


const globalsBtn = document.querySelector("#globalsButton")
const globals = async () => {
  let n = parseInt(document.querySelector("#stateGlobalsFor").value)
  let r = await c.getStateGlobals(n)
  appendToLog(JSON.stringify(r,null,2))

}
globalsBtn.addEventListener("click",globals)


const actionsBtn = document.querySelector("#actionsButton")
const actions = async () => {
  let n = parseInt(document.querySelector("#stateActionsFor").value)
  let r = await c.getStateActions(n)
  appendToLog(r)

}
actionsBtn.addEventListener("click",actions)


const statesBtn = document.querySelector("#statesButton")
const states = async () => {
  let r = await c.getStates()
  appendToLog(r)

}
statesBtn.addEventListener("click",states)


const statusBtn = document.querySelector("#statusButton")
const status = async () => {
  let r = await c.getStatus()
  appendToLog(r)

}
statusBtn.addEventListener("click",status)


const respondBtn = document.querySelector("#respondButton")
const respond = async () => {
  let s = parseInt(document.querySelector("#resForState").value)
  let a = parseInt(document.querySelector("#resForAction").value)
  let v = parseInt(document.querySelector("#resForVal").value)
  let w = parseInt(document.querySelector("#resForActor").value)
  let r = await c.respondWithVal(s,a,v,w)
  appendToLog(r)
  redraw()

}
respondBtn.addEventListener("click",respond)


const initForBtn = document.querySelector("#initForButton")
const initFor = async () => {
  let s = parseInt(document.querySelector("#initForState").value)
  let a = parseInt(document.querySelector("#initForActor").value)
  let r = await c.initFor(s,a)
  appendToLog(r)
  redraw()

}
initForBtn.addEventListener("click",initFor)


const initBtn = document.querySelector("#initButton")
const init = async () => {
  let r = await c.init()
  appendToLog(r)
  redraw()

}
initBtn.addEventListener("click",init)


const loadBtn = document.querySelector("#loadButton")
const load = async () => {
  let r = await c.load()
  appendToLog(r)

}
loadBtn.addEventListener("click",load)


const pingBtn = document.querySelector("#pingButton")
const ping = async () => {
  let r = await c.ping()
  appendToLog(r)
}
pingBtn.addEventListener("click",ping)

const appendToLog = (r) => {
  let x = log.innerHTML
  log.innerHTML = x + '<br>' + r
}
