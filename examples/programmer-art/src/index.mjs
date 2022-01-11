import * as c from '@reach-sh/simulator-client';
import cytoscape from 'cytoscape';
import klay from 'cytoscape-klay';
import Prism from 'prismjs';
import "../scss/custom.scss";

// NOTE: placeholder
const rsh = `'reach 0.1';

const Player = {
  getHand: Fun([], UInt),
  seeOutcome: Fun([UInt], Null),
};

export const main = Reach.App(() => {
  const Alice = Participant('Alice', {
    ...Player,
  });
  const Bob   = Participant('Bob', {
    ...Player,
  });
  deploy();

  Alice.only(() => {
    const handAlice = declassify(interact.getHand());
  });
  Alice.publish(handAlice);
  commit();

  Bob.only(() => {
    const handBob = declassify(interact.getHand());
  });
  Bob.publish(handBob);

  const outcome = (handAlice + (4 - handBob)) % 3;
  commit();

  each([Alice, Bob], () => {
    interact.seeOutcome(outcome);
  });
});
`

cytoscape.use( klay );

const srcHtml = Prism.highlight(rsh, Prism.languages.javascript, 'javascript');
const prism = document.querySelector("#cx")
prism.innerHTML = srcHtml

const spa = document.querySelector("#spa")

const renderObjects = async (nodeId) => {
  const r = await c.getStateLocals(nodeId)
  let obs = ``
  for (const [k,v] of Object.entries(r.l_locals)) {
    console.log(k, v);
    let who = v.l_who ? v.l_who : 'Consensus'
    let status = 'Initial'
    switch (v.l_ks) {
      case 'PS_Suspend':
        status = 'Running'
        break;
      case 'PS_Done':
        status = 'Done'
        break;
    }
    obs = obs + `
      <button type="button"
      class="list-group-item list-group-item-action">
      ${who}
      <span class="badge bg-secondary">${status}</span>
      </button> `
  }
  console.log(r)
  spa.innerHTML = `
  <ul class="list-group list-group-flush">
    ${obs}
  </ul>
  `
}

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
  cy.bind('click', 'node', function(evt) {
    const nodeId = evt.target.id()
    renderObjects(nodeId)
  });
}

const log = document.querySelector("#output")
const jsonLog = []

const localsBtn = document.querySelector("#localsButton")
const locals = async () => {
  let n = parseInt(document.querySelector("#stateLocalsFor").value)
  let r = await c.getStateLocals(n)
  appendToLog(JSON.stringify(r,null,2))
  jsonLog.push(["getStateLocals",n])
}
localsBtn.addEventListener("click",locals)


const globalsBtn = document.querySelector("#globalsButton")
const globals = async () => {
  let n = parseInt(document.querySelector("#stateGlobalsFor").value)
  let r = await c.getStateGlobals(n)
  appendToLog(JSON.stringify(r,null,2))
  jsonLog.push(["getStateGlobals",n])
}
globalsBtn.addEventListener("click",globals)


const actionsBtn = document.querySelector("#actionsButton")
const actions = async () => {
  let n = parseInt(document.querySelector("#stateActionsFor").value)
  let r = await c.getStateActions(n)
  appendToLog(r)
  jsonLog.push(["getStateActions",n])
}
actionsBtn.addEventListener("click",actions)


const statesBtn = document.querySelector("#statesButton")
const states = async () => {
  let r = await c.getStates()
  appendToLog(r)
  jsonLog.push(["getStates"])
}
statesBtn.addEventListener("click",states)


const statusBtn = document.querySelector("#statusButton")
const status = async () => {
  let r = await c.getStatus()
  appendToLog(r)
  jsonLog.push(["getStatus"])
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
  jsonLog.push(["respondWithVal",s,a,v,w])
}
respondBtn.addEventListener("click",respond)


const initForBtn = document.querySelector("#initForButton")
const initFor = async () => {
  let s = parseInt(document.querySelector("#initForState").value)
  let a = parseInt(document.querySelector("#initForActor").value)
  let r = await c.initFor(s,a)
  appendToLog(r)
  redraw()
  jsonLog.push(["initFor",s,a])
}
initForBtn.addEventListener("click",initFor)


const initBtn = document.querySelector("#initButton")
const init = async () => {
  let r = await c.init()
  appendToLog(r)
  redraw()
  jsonLog.push(["init"])
}
initBtn.addEventListener("click",init)


const loadBtn = document.querySelector("#loadButton")
const load = async () => {
  let r = await c.load()
  appendToLog(r)
  jsonLog.push(["load"])
}
loadBtn.addEventListener("click",load)


const pingBtn = document.querySelector("#pingButton")
const ping = async () => {
  let r = await c.ping()
  appendToLog(r)
  jsonLog.push(["ping"])

}
pingBtn.addEventListener("click",ping)

const resetBtn = document.querySelector("#resetButton")
const reset = async () => {
  let r = await c.resetServer()
  appendToLog(r)
  jsonLog.push(["reset"])
}
resetBtn.addEventListener("click",reset)

const appendToLog = (r) => {
  let x = log.innerHTML
  log.innerHTML = x + '<br>' + '$ ' + r
}

const printBtn = document.querySelector("#printButton")
const printLog = () => {
  console.log(jsonLog);
  console.log(JSON.stringify(jsonLog));
}
printBtn.addEventListener("click",printLog)

const jsonBtn = document.querySelector("#inputJsonButton")
const runJsonScript = async () => {
  let j = document.querySelector("#inputJsonScript").value
  await c.resetServer()
  await c.interp(JSON.parse(j))
  redraw()
}
jsonBtn.addEventListener("click",runJsonScript)
