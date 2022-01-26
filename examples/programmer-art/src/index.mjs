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

const noop = () => {
 appendToLog('No-Op');
}

const noOpHandler = (evt) => {
  noop();
}

await c.resetServer()
const rsh = await c.load()
const hlns1 = document.createElement('script');
const hlns2 = document.createElement('script');
const hlns3 = document.createElement('script');
hlns1.src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.4.0/highlight.min.js"
hlns1.async = false
hlns2.src = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.4.0/languages/javascript.min.js"
hlns2.async = false
hlns3.src = "https://cdnjs.cloudflare.com/ajax/libs/highlightjs-line-numbers.js/2.8.0/highlightjs-line-numbers.min.js";
hlns3.async = false
hlns1.onload = function() {
  document.getElementsByTagName('head')[0].appendChild(hlns2);
};
hlns2.onload = function() {
  document.getElementsByTagName('head')[0].appendChild(hlns3);
};
hlns3.onload = function() {
  hljs.highlightAll();
  hljs.initLineNumbersOnLoad();
};
document.getElementsByTagName('head')[0].appendChild(hlns1);

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
  let actorSet = {}
  for (const [k,v] of Object.entries(r.l_locals)) {
    const who = v.l_who ? v.l_who : 'Consensus'
    actorSet[k] = who
  }
  let actors = ``
  for (const [k,v] of Object.entries(actorSet)) {
    actors = actors + `<option value="${k}">${v}</option>`
  }
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
      data-node-id="${nodeId}"
      data-actor-set='${JSON.stringify(actorSet)}'>
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
    <button type="button" id="localsButton" data-node-id="${nodeId}" class="list-group-item list-group-item-action">Get State Locals</button>
    <button type="button" id="globalsButton" data-node-id="${nodeId}" class="list-group-item list-group-item-action">Get State Globals</button>
    <div class="d-flex justify-content-center">
      <select name="actors" id="actors-spa-select">
        ${actors}
      </select>
      <button type="button" id="initForButton" data-node-id="${nodeId}" class="btn btn-outline-secondary btn-sm">Init For Actor</button>
    </div>
    <li></li>
  </ul>
  `
  bindObjDetailsEvents();
}

const bindObjDetailsEvents = () => {
  const objectBtns = document.querySelectorAll(".object-button")
  objectBtns.forEach((item, i) => {
    item.addEventListener("click",renderObjectDetails)
  });
  const localsBtn = document.querySelector("#localsButton")
  const locals = async (evt) => {
    const tgt = evt.target.closest("#localsButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.getStateLocals(nodeId)
    appendToLog(JSON.stringify(r,null,2))
    jsonLog.push(["getStateLocals",nodeId])
  }
  localsBtn.addEventListener("click",locals)

  const globalsBtn = document.querySelector("#globalsButton")
  const globals = async (evt) => {
    const tgt = evt.target.closest("#globalsButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.getStateGlobals(nodeId)
    appendToLog(JSON.stringify(r,null,2))
    jsonLog.push(["getStateGlobals",nodeId])
  }
  globalsBtn.addEventListener("click",globals)

  const initForBtn = document.querySelector("#initForButton")
  const initFor = async (evt) => {
    const tgt = evt.target.closest("#initForButton")
    const nodeId = tgt.dataset.nodeId
    let e = document.querySelector("#actors-spa-select");
    let selectedActorId = e.value;
    let r = await c.initFor(nodeId,selectedActorId)
    appendToLog(r)
    redraw()
    jsonLog.push(["initFor",nodeId,selectedActorId])
  }
  initForBtn.addEventListener("click",initFor)
}

const renderObjectDetails = async (evt) => {
  objectsHTML = spa.innerHTML
  const tgt = evt.target.closest(".object-button")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const actorSet = tgt.dataset.actorSet
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
    data-actor-set='${actorSet}'
    data-who="${who}">
    <div class="badge bg-secondary">Status</div>
    <div> ${status} </div>
    </button> `
  for (const [k,v] of Object.entries(ledger[actorId])) {
    dets = dets + `
      <button type="button"
      class="list-group-item list-group-item-action object-button no-op"
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
      class="list-group-item list-group-item-action object-button no-op"
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
  const noOps = document.querySelectorAll(".no-op")
  noOps.forEach((item, i) => {
    item.addEventListener("click",noOpHandler)
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
  const actorSet = tgt.dataset.actorSet
  const who = tgt.dataset.who
  const act = await c.getActions(nodeId,actorId)
  console.log(act)
  if (!act) {
    noop();
    return false
  }
  let acts = ``
  acts = acts + renderAction(act,nodeId,actorId,who,actorSet)
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
  const actorSet = JSON.parse(tgt.dataset.actorSet)
  let actors = `<option value="">Unchanged</option>`
  for (const [k,v] of Object.entries(actorSet)) {
    actors = actors + `<option value="${k}">${v}</option>`
  }
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
    <select name="actors" id="actors-spa-select">
    ${actors}
    </select>
    <button type="button" id="spa-res-button" class="btn btn-outline-secondary btn-sm">Respond</button>
  </div>
  `
  const spaRespondBtn = document.querySelector("#spa-res-button")
  const respondSpa = async () => {
    let v = parseInt(document.querySelector("#spa-response").value)
    let e = document.querySelector("#actors-spa-select");
    let selectedActorId = e.value;
    let aid = actorId
    if (selectedActorId) {
      aid = parseInt(selectedActorId)
    }
    let r = await c.respondWithVal(nodeId,actId,v,aid)
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

const renderAction = (actObj,nodeId,actorId,who,actorSet) => {
  const act = actObj[1]
  const actId = actObj[0]
  const common = `
  <button type="button"
  class="list-group-item list-group-item-action action-button"
  data-act-id="${actId}"
  data-actor-id="${actorId}"
  data-node-id="${nodeId}"
  data-actor-set='${actorSet}'
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

    if (size == 1) {
      // single "frame", cancel animation
      nodes[0].style('visibility', 'visible')
      return;
    }

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

const initBtn = document.querySelector("#initButton")
const init = async () => {
  let r = await c.init()
  appendToLog(r)
  redraw()
  jsonLog.push(["init"])
}
initBtn.addEventListener("click",init)


const printBtn = document.querySelector("#printButton")
const printLog = () => {
  console.log(jsonLog);
  console.log(JSON.stringify(jsonLog));
}
printBtn.addEventListener("click",printLog)

const jsonBtn = document.querySelector("#inputJsonButton")
const runJsonScript = async () => {
  let j = document.querySelector("#inputJsonScript").value
  await c.interp(JSON.parse(j))
  redraw()
}
jsonBtn.addEventListener("click",runJsonScript)
