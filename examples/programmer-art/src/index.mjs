import * as c from '@reach-sh/simulator-client';
import cytoscape from 'cytoscape';
import klay from 'cytoscape-klay';
import "../scss/custom.scss";

const log = document.querySelector("#output")
const jsonLog = []

const appendToLog = (r) => {
  let x = log.innerHTML
  log.innerHTML = x + '<br>' + '$ ' + r
}

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

const sheet = (function() {
	const style = document.createElement("style");
	style.appendChild(document.createTextNode(""));
	document.head.appendChild(style);
	return style.sheet;
})();

const codeDiv = document.querySelector("#cx")
codeDiv.innerHTML = rsh

const spa = document.querySelector("#spa")

let objectsHTML = null;
let detailsHTML = null;
let actionsHTML = null;

const renderObjects = async (nodeId) => {
  const r = await c.getStateLocals(nodeId)
  let obs = ``
  for (const [k,v] of Object.entries(r.l_locals)) {
    const who = v.l_who ? v.l_who : 'Consensus'
    let status = 'Initial'
    switch (v.l_ks) {
      case 'PS_Suspend':
        status = 'Program Running'
        break;
      case 'PS_Done':
        status = 'Program Done'
        break;
    }
    obs = obs + `
      <button type="button"
      class="list-group-item list-group-item-action object-button"
      data-actor-id="${k}"
      data-node-id="${nodeId}">
      ${who}
      <span class="badge bg-secondary">${status}</span>
      </button> `
  }
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item active" aria-current="page">Objects</li>
    </ol>
  </nav>
  <ul class="list-group list-group-flush">
    ${obs}
  </ul>
  `
  bindObjDetailsEvents();
}

const bindObjDetailsEvents = () => {
  const objectBtns = document.querySelectorAll(".object-button")
  objectBtns.forEach((item, i) => {
    item.addEventListener("click",renderObjectDetails)
  });
}

const renderObjectDetails = async (evt) => {
  objectsHTML = spa.innerHTML
  const tgt = evt.target.closest(".object-button")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const r = await c.getStateLocals(nodeId)
  const g = await c.getStateGlobals(nodeId)
  const detsj = r.l_locals[actorId]
  const ledger = g.e_ledger
  let dets = ``
  let status = 'Initial'
  switch (detsj.l_ks) {
    case 'PS_Suspend':
      status = 'Running'
      break;
    case 'PS_Done':
      status = 'Done'
      break;
  }
  const who = detsj.l_who ? detsj.l_who : 'Consensus'
  dets = dets + `
    <button type="button"
    class="list-group-item list-group-item-action
    object-button status-panel"
    data-actor-id="${actorId}"
    data-node-id="${nodeId}"
    data-who="${who}">
    <div class="badge bg-secondary">Status</div>
    <div> ${status} </div>
    </button> `
  for (const [k,v] of Object.entries(ledger[actorId])) {
    dets = dets + `
      <button type="button"
      class="list-group-item list-group-item-action object-button"
      >
      <div class="badge bg-secondary">Funds</div>
      <div> ${v} (Token ID: ${k}) </div>
      </button> `
  }
  for (const [varName,varDetails] of detsj.l_store) {
    const typing = varDetails.tag
    const varValue = varDetails.contents
    dets = dets + `
      <button type="button"
      class="list-group-item list-group-item-action object-button"
      >
      <div class="badge bg-secondary">${typing.slice(2)}</div>
      <div> ${varName} := ${varValue} </div>
      </button> `
  }
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects">Objects</a></li>
      <li class="breadcrumb-item active" aria-current="page">Details (${who})</li>
    </ol>
  </nav>
  <ul class="list-group list-group-flush">
    ${dets}
  </ul>
  `
  bindObjDetailsActionsEvents()
  setupReturnToObjects()
}

const bindObjDetailsActionsEvents = () => {
  const statusPanel = document.querySelectorAll(".status-panel")
  statusPanel.forEach((item, i) => {
    item.addEventListener("click",detailActions)
  });
}

const setupReturnToObjects = () => {
  const objectsRetLink = document.querySelector("#return-to-objects")
  const backtrackToObjects = () => {
    if (objectsHTML) {
      spa.innerHTML = objectsHTML
      objectsHTML = null;
      bindObjDetailsEvents();
    }
  }
  objectsRetLink.addEventListener("click",backtrackToObjects)
}

const setupReturnToObjectsDetails = () => {
  const detsRetLink = document.querySelector("#return-to-details")
  const backtrackToDetails = () => {
    if (detailsHTML) {
      spa.innerHTML = detailsHTML
      detailsHTML = null;
      bindObjDetailsActionsEvents()
      setupReturnToObjects()
    }
  }
  detsRetLink.addEventListener("click",backtrackToDetails)
}

const detailActions = async (evt) => {
  detailsHTML = spa.innerHTML
  const tgt = evt.target.closest(".status-panel")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const who = tgt.dataset.who
  const act = await c.getActions(nodeId,actorId)
  console.log(act)
  if (!act) {
    return false
  }
  let acts = ``
  acts = acts + renderAction(act,nodeId,actorId,who)
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects">Objects</a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-details">Details (${who})</a></li>
      <li class="breadcrumb-item active" aria-current="page">Actions</li>
    </ol>
  </nav>
  <ul class="list-group list-group-flush">
    ${acts}
  </ul>
  `
  bindObjDetailsActionsResponseEvents()
  setupReturnToObjects()
  setupReturnToObjectsDetails()
}

const bindObjDetailsActionsResponseEvents = () => {
  const actionsPanel = document.querySelectorAll(".action-button")
  actionsPanel.forEach((item, i) => {
    item.addEventListener("click",respondToActions)
  });
}


const respondToActions = async (evt) => {
  actionsHTML = spa.innerHTML
  const tgt = evt.target.closest(".action-button")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const actId = parseInt(tgt.dataset.actId)
  const who = tgt.dataset.who
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects">Objects</a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-details">Details (${who})</a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-actions">Actions</a></li>
      <li class="breadcrumb-item active" aria-current="page">Response</li>
    </ol>
  </nav>
  <div class="bordered d-flex justify-content-center">
    <input type="text" id="spa-response" class="form-control form-control-sm" placeholder="Value">
    <button type="button" id="spa-res-button" class="btn btn-outline-secondary btn-sm">Respond</button>
  </div>
  `
  const spaRespondBtn = document.querySelector("#spa-res-button")
  const respondSpa = async () => {
    let v = parseInt(document.querySelector("#spa-response").value)
    let r = await c.respondWithVal(nodeId,actId,v,actorId)
    appendToLog(r)
    redraw()
    jsonLog.push(["respondWithVal",nodeId,actId,v,actorId])
  }
  spaRespondBtn.addEventListener("click",respondSpa)

  setupReturnToObjects()
  setupReturnToObjectsDetails()
  setupReturnToActions()
}

const setupReturnToActions = () => {
  const actionsRetLink = document.querySelector("#return-to-actions")
  const backtrackToActions = () => {
    if (actionsHTML) {
      spa.innerHTML = actionsHTML
      actionsHTML = null;
      bindObjDetailsActionsEvents();
      setupReturnToObjects()
      setupReturnToObjectsDetails()
    }
  }
  actionsRetLink.addEventListener("click",backtrackToActions)
}

const renderAction = (actObj,nodeId,actorId,who) => {
  const act = actObj[1]
  const actId = actObj[0]
  const common = `
  <button type="button"
  class="list-group-item list-group-item-action action-button"
  data-act-id="${actId}"
  data-actor-id="${actorId}"
  data-node-id="${nodeId}"
  data-who="${who}">
  <div class="badge bg-secondary">
  `
  switch (act.tag) {
    case 'A_Interact':
      let domain = `Null`
      if (act.contents[4].length != 0) {
        domain = act.contents[4].map(x => x.tag.slice(2)).join(',')
      }
      return `
        ${common}
        ${act.tag.slice(2)}</div>
        <div> ${act.contents[2]} : ${domain} &#8594; ${act.contents[3].tag.slice(2)} </div>
        </button> `
    case 'A_TieBreak':
      return `
        ${common}${act.tag.slice(2)}</div>
        <div> Phase Id: ${act.contents[0]}, Actors: ${act.contents[1].join(',')} </div>
        </button> `
    case 'A_InteractV':
      return `
        ${common}${act.tag.slice(2)}</div>
        <div> ${act.contents[0]},${act.contents[1]}</div>
        </button> `
    case 'A_Remote':
      return `
        ${common}${act.tag.slice(2)}</div>
        </button> `
    case 'A_Contest':
      return `
        ${common}${act.tag.slice(2)}</div>
        <div> Phase Id: ${act.contents}</div>
        </button> `
    case 'A_AdvanceTime':
      return `
        ${common}${act.tag.slice(2)}</div>
        <div> Time: ${act.contents}</div>
        </button> `
    case 'A_AdvanceSeconds':
      return `
        ${common}${act.tag.slice(2)}</div>
        <div> Seconds: ${act.contents}</div>
        </button> `
    case 'A_None':
      return `
        ${common}${act.tag.slice(2)}</div>
        </button> `
    default:
      console.log("Unexpected Action Encountered")
  }

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
          'label': 'data(id)',
          'visibility': 'hidden'
        }
      },
      {
        selector: 'edge',
        style: {
          'width': 3,
          'line-color': '#ccc',
          'target-arrow-color': '#ccc',
          'target-arrow-shape': 'triangle',
          'curve-style': 'bezier',
          'visibility': 'hidden'
        }
      }
    ],
    layout: {
      name: 'klay'
    }
  });
  cy.bind('click', 'node', clickNode);
  const eles = cy.filter(function(element, i){
    return true;
  });
  // citation: animation adapted from https://gist.github.com/maxkfranz/aedff159b0df05ccfaa5
  // and https://stackoverflow.com/questions/40096407/animate-building-a-graph-in-cytoscape-js
  const animateGraph = (nodes) => {
    var delay = 0;
    var size = nodes.length;
    var duration = (1000 / size);
    var visitedMap = {};

    for(var index = 0; index < size; ++index){
      visitedMap[nodes[index].data('id')] = 0;
    }
    var nodesCopy = nodes.clone();

    for( var i = 0; i < nodes.length; ++i ){
      var cNode = nodes[i];
      var nextNodes = cNode.connectedEdges(
        function(o){
          return o.source().same(cNode);
        }
      ).targets();
      const move = (nextNode,currentNode,copyNode,ifCurr) => {
        const position = nextNode.renderedPosition();
        nextNode.renderedPosition(copyNode.renderedPosition());
        nextNode.delay(delay, function(){
          if (ifCurr) {
            currentNode.style("visibility", "visible");
          }
          nextNode.style('visibility', 'visible');
          nextNode.connectedEdges().style("visibility", "visible");
        } ).animate({
              renderedPosition: position,
            }, {
              duration: duration,
              complete: function(){}
          }
        );
      }
      for (var index = 0; index < nextNodes.length; ++index){
        var nNode = nextNodes[index];
        (function(currentNode, x, copyNode, nextNode){
          if(nextNode != null && x != 0 && visitedMap[nextNode.data('id')] < 1){
            ++visitedMap[nextNode.data('id')];
            console.log('currentNode: ' + currentNode.data('id')+ ', x: ' + x + ', nextNode: ' + nextNode.data('id') );
            move(nextNode,currentNode,copyNode,false)
          } else if (nextNode != null && visitedMap[nextNode.data('id')] < 1){
            ++visitedMap[nextNode.data('id')];
            move(nextNode,currentNode,copyNode,true)
          }
          delay += duration;
          })(cNode, i, nodesCopy[i], nNode);
      }
    }
  };
  animateGraph(eles)
}

const clickNode = async (evt) => {
  const nodeId = evt.target.id()
  const at = await c.getLoc(nodeId)
  if (sheet.cssRules.length > 0) {
    sheet.deleteRule(0);
  }
  if (at) {
    const n = at.split(':')[1]
    sheet.insertRule(`.hljs-ln-line[data-line-number="${n}"] {
      background-color: silver;
    }`);
    const poi = document.querySelector(`.hljs-ln-line[data-line-number="${n}"`);
    const topPos = poi.offsetTop;
    document.querySelector('.code-container').scrollTop = topPos;
  }
  console.log(at)
  renderObjects(nodeId)
}



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
  let s = parseInt(document.querySelector("#actionsForState").value)
  let a = parseInt(document.querySelector("#actionsForActor").value)
  let r = await c.getActions(s,a)
  appendToLog(JSON.stringify(r,null,2))
  jsonLog.push(["getActions",s,a])
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
