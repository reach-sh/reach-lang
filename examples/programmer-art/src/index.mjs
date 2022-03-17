import * as c from '@reach-sh/simulator-client';
import cytoscape from 'cytoscape';
import klay from 'cytoscape-klay';
import "../scss/custom.scss";

// initialize the JSON Log
let jsonLog = [
  ["resetServer"],
  ["load"],
  ["init"]
]

// reset server
await c.resetServer()

// load the Reach program
const rsh = await c.load()

// initialize the program for the Consensus
await c.init()

// load highlight.js to render Reach code
// https://highlightjs.org/usage/

// there is a plug-in for the line numbers
// https://github.com/wcoder/highlightjs-line-numbers.js/

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

// we use cytoscape.js for the graph
// currently we use the "klay" layout
cytoscape.use( klay );

// currently we use dynamic css stylesheets to help
// with highlighting line numbers
const sheet = (function() {
	const style = document.createElement("style");
	style.appendChild(document.createTextNode(""));
	document.head.appendChild(style);
	return style.sheet;
})();

// Reach code display tile
const codeDiv = document.querySelector("#cx")
codeDiv.innerHTML = rsh

// the Single Page Application (state details) tile
const spa = document.querySelector("#spa")

// currently the SPA works by saving "views" to these variables
// these represent the first 3 views in that order

// The first view is the objects view which lists the Actors, lets you do
// transfers, add new accounts/tokens etc.

// Each object should have a details view.

// Each detail should have an actions view.

// Each action should have a response view, but since that is the
// terminal view, it doesn't have to be saved for the breadcrumb UI.

let objectsHTML = null;
let detailsHTML = null;
let actionsHTML = null;


//#### event handler which loads the Objects View
const renderObjects = async (nodeId) => {
  const r = await c.getStateLocals(nodeId)
  const apis = await c.getAPIs()
  const views = await c.getViews(nodeId)
  let obs = ``
  let apiBs = ``
  let viewBs = ``
  let actorSet = {}
  let apiSet = {}
  let viewSet = {}
  for (const [k,v] of Object.entries(r.l_locals)) {
    const who = v.l_who ? v.l_who : 'Consensus'
    actorSet[k] = who
  }
  for (const [k,v] of Object.entries(apis)) {
    const who = v.a_name
    apiSet[k] = who
  }
  for (const [k,v] of Object.entries(views)) {
    viewSet[k] = k
  }
  let actors = ``
  let actorsNoCons = ``
  const actorEntries = Object.entries(actorSet)
  // NOTE: assumption: there is at least one non-consensus actor
  const firstActorId = actorEntries[0][0]
  for (const [k,v] of actorEntries) {
    actors = actors + `<option value="${k}">${v}</option>`
  }
  for (const [k,v] of actorEntries) {
    if (parseInt(k) !== -1 ) {
      actorsNoCons = actorsNoCons + `<option value="${k}">${v}</option>`
    }
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
      data-actor-set='${JSON.stringify(actorSet)}'
      data-api-set='${JSON.stringify(apiSet)}'>
      ${who}
      <span class="badge bg-secondary">${status}</span>
      </button> `
  }

  for (const [k,v] of Object.entries(apis)) {
    let status = 'Initial'
    let f = v.a_liv.out.contents[0][1].contents
    apiBs = apiBs + `
      <button type="button"
      class="list-group-item list-group-item-action api-button"
      data-node-id="${nodeId}"
      data-api-id="${k}"
      data-api-name="${v.a_name}"
      data-fn="${JSON.stringify(f)}">
      ${v.a_name}
      </button> `
  }

  for (const [k,v] of Object.entries(views)) {
    viewBs = viewBs + `
      <button type="button"
      class="list-group-item list-group-item-action view-button"
      data-node-id="${nodeId}"
      data-view-id="${k}"
      data-view-var="${v.v_var}"
      data-view-tag="${v.v_ty.tag}"
      data-view-contents="${v.v_ty.contents}"
      data-view-name="${v.v_name}">
      ${v.v_name}:${v.v_var}
      </button> `
  }

  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item active" aria-current="page"><span class="omph">Objects (${nodeId})</span></li>
    </ol>
  </nav>
  <ul class="list-group list-group-flush">
    ${obs}


    <div class="extra-margin-bottom">
      <h4>Accounts/Tokens</h4>
    </div>

    <button type="button" id="newAccButton" data-node-id="${nodeId}" class="top list-group-item list-group-item-action">New Account <i class="bi bi-plus-lg"></i></button>
    <button type="button" id="newTokButton" data-node-id="${nodeId}" class="list-group-item list-group-item-action">New Token <i class="bi bi-plus-lg"></i></button>

    <div>
      <h4>Actor Initialization</h4>
      <div class="pad-me d-flex justify-content-center">
        <div>
          <select name="init-actors" id="init-actors-spa-select">
            ${actorsNoCons}
          </select>
        </div>
        <div id="initDetailsPanel" class="pad-me d-flex justify-content-center">
        </div>
        <input type="text" id="init-account" class="form-control form-control-sm" placeholder="Account ID (optional)">
        <div>
          <button type="button" id="initForButton" data-node-id="${nodeId}" class="btn btn-outline-light btn-sm">Init Actor</button>
        </div>
      </div>
    </div>

    <hr>
    <div>
      <h4>Transfer Funds</h4>

      <div class="pad-me d-flex justify-content-center shrink-text">
        SND:
        <select name="actors-transfer-from" id="actors-spa-select-transfer-from">
          ${actors}
        </select>
        RCV:
        <select name="actors-transfer-to" id="actors-spa-select-transfer-to">
          ${actors}
        </select>
      </div>
      <div class="pad-me d-flex justify-content-center">
        <input type="text" id="token-id" class="form-control form-control-sm" placeholder="Token Id">
        <input type="text" id="transfer-amount" class="form-control form-control-sm" placeholder="Amount">

        <button type="button" id="transferButton" data-node-id="${nodeId}" class="btn btn-outline-light btn-sm">Transfer  <i class="bi bi-arrow-right-circle"></i></button>

      </div>
    </div>

    <hr>
    <div>
      <h4>APIs</h4>
      <ul class="list-group list-group-flush">
        ${apiBs}
      </ul>
    </div>

    <hr>
    <div>
      <h4>Views</h4>
      <ul class="list-group list-group-flush">
        ${viewBs}
      </ul>
    </div>

    <hr>
    <div>
      <h4>Logging</h4>
      <button type="button" id="localsButton" data-node-id="${nodeId}" class="list-group-item list-group-item-action">Get State Locals <i class="bi bi-clipboard"></i></button>
      <button type="button" id="globalsButton" data-node-id="${nodeId}" class="list-group-item list-group-item-action">Get State Globals <i class="bi bi-clipboard"></i></button>
    </div>

  </ul>
  `
  initActorDetsHelper(firstActorId);
  bindObjDetailsEvents();
}

// helper for the Actor Initialization form (on the Objects View)
const initActorDetsHelper = async (a) => {
  const initPanel = document.querySelector("#initDetailsPanel")
  const dets = await c.initDetails(a)
  let initHtml = ``
  for (const [k,v] of Object.entries(dets)) {
    let vDisplay = v.slice(7)
    if (vDisplay.startsWith('Bytes')) {
      vDisplay = 'Bytes'
    }
    initHtml = initHtml + `
    <div class="pad-me d-flex justify-content-center shrink-text">
      <div class="pad-me"> ${k} <span class="badge bg-secondary"> ${vDisplay} </span> </div>
      <input type="text" data-init-val="${k}" data-init-type="${vDisplay}" class="form-control form-control-sm init-detail" placeholder="Value">
    </div>`
  }
  initPanel.innerHTML = initHtml
}

// helper to bind events for the Objects View
const bindObjDetailsEvents = () => {
  const objectBtns = document.querySelectorAll(".object-button")
  objectBtns.forEach((item, i) => {
    item.addEventListener("click",renderObjectDetails)
  });

  const apiBtns = document.querySelectorAll(".api-button")
  apiBtns.forEach((item, i) => {
    item.addEventListener("click",clickAPIButton)
  });

  const viewBtns = document.querySelectorAll(".view-button")
  viewBtns.forEach((item, i) => {
    item.addEventListener("click",clickViewButton)
  });

  const newAccBtn = document.querySelector("#newAccButton")
  const newAccHandler = async (evt) => {
    const tgt = evt.target.closest("#newAccButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.newAccount(nodeId)
    jsonLog.push(["newAccount",nodeId])

    let fr = JSON.stringify(r,null,2)
    let icon = evt.target.querySelector('.bi')
    if (!icon) {
      icon = evt.target
    }
    icon.classList.remove('bi-plus-lg')
    icon.classList.add('bi-check2')
    await new Promise(resolve => setTimeout(resolve, 1000))
    await navigator.clipboard.writeText(fr)
    alert(`New Account (ID: ${fr}) created `);
    icon.classList.remove('bi-check2')
    icon.classList.add('bi-plus-lg')
    console.log(`added new Account id: ${r}`)
  }
  newAccBtn.addEventListener("click",newAccHandler)

  const newTokBtn = document.querySelector("#newTokButton")
  const newTokHandler = async (evt) => {
    const tgt = evt.target.closest("#newTokButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.newToken(nodeId)
    jsonLog.push(["newToken",nodeId])

    let fr = JSON.stringify(r,null,2)
    let icon = evt.target.querySelector('.bi')
    if (!icon) {
      icon = evt.target
    }
    icon.classList.remove('bi-plus-lg')
    icon.classList.add('bi-check2')
    await new Promise(resolve => setTimeout(resolve, 1000))
    await navigator.clipboard.writeText(fr)
    alert(`New Token (ID: ${fr}) created `);
    icon.classList.remove('bi-check2')
    icon.classList.add('bi-plus-lg')
    console.log(`added new Token id: ${r}`)
  }
  newTokBtn.addEventListener("click",newTokHandler)

  const localsBtn = document.querySelector("#localsButton")
  const locals = async (evt) => {
    const tgt = evt.target.closest("#localsButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.getStateLocals(nodeId)
    let fr = JSON.stringify(r,null,2)
    let icon = evt.target.querySelector('.bi')
    if (!icon) {
      icon = evt.target
    }
    icon.classList.remove('bi-clipboard')
    icon.classList.add('bi-check2')
    await new Promise(resolve => setTimeout(resolve, 1000))
    await navigator.clipboard.writeText(fr)
    icon.classList.remove('bi-check2')
    icon.classList.add('bi-clipboard')
    console.log("logged local state")
  }
  localsBtn.addEventListener("click",locals)

  const globalsBtn = document.querySelector("#globalsButton")
  const globals = async (evt) => {
    const tgt = evt.target.closest("#globalsButton")
    const nodeId = tgt.dataset.nodeId
    let r = await c.getStateGlobals(nodeId)
    let fr = JSON.stringify(r,null,2)
    let icon = evt.target.querySelector('.bi')
    if (!icon) {
      icon = evt.target
    }
    icon.classList.remove('bi-clipboard')
    icon.classList.add('bi-check2')
    await new Promise(resolve => setTimeout(resolve, 1000))
    await navigator.clipboard.writeText(fr)
    icon.classList.remove('bi-check2')
    icon.classList.add('bi-clipboard')
    console.log("logged global state")
  }
  globalsBtn.addEventListener("click",globals)

  const initActorSlct = document.querySelector("#init-actors-spa-select")
  const initActorDets = async (evt) => {
    initActorDetsHelper(evt.target.value)
  }
  initActorSlct.addEventListener("change",initActorDets)

  const initForBtn = document.querySelector("#initForButton")
  const initFor = async (evt) => {
    const tgt = evt.target.closest("#initForButton")
    const nodeId = parseInt(tgt.dataset.nodeId)
    let e = document.querySelector("#init-actors-spa-select");
    let selectedActorId = parseInt(e.value);
    const dets = document.querySelectorAll(".init-detail")
    const liv = {}
    for (const det of dets) {
      let type = `V_` + det.dataset.initType
      let enter = det.value
      if (['UInt','Token','Address','Contract'].includes(det.dataset.initType)) {
        enter = parseInt(enter)
      }
      liv[det.dataset.initVal] = {"tag": type, "contents": enter}
    }
    let accId = document.querySelector("#init-account").value
    if (accId) {
      accId = parseInt(accId)
    }

    let r = await c.initFor(nodeId,selectedActorId,JSON.stringify(liv),accId)
    redraw()
    jsonLog.push(["initFor",nodeId,selectedActorId,JSON.stringify(liv),accId])
  }
  initForBtn.addEventListener("click",initFor)

  let toSelLen = document.querySelector("#actors-spa-select-transfer-to").children.length
  if (toSelLen > 1) {
    let secondOpt = document.querySelector("#actors-spa-select-transfer-to").children[1]
    secondOpt.setAttribute('selected','')
  }
  const transferBtn = document.querySelector("#transferButton")
  const transfer = async (evt) => {
    const tgt = evt.target.closest("#transferButton")
    const nodeId = tgt.dataset.nodeId
    let frSelect = document.querySelector("#actors-spa-select-transfer-from");
    let frActorId = frSelect.value;
    let toSelect = document.querySelector("#actors-spa-select-transfer-to");
    let toActorId = toSelect.value;
    let tokId = parseInt(document.querySelector("#token-id").value)
    let amount = parseInt(document.querySelector("#transfer-amount").value)
    let icon = evt.target.querySelector('.bi')
    if (!icon) {
      icon = evt.target
    }
    icon.classList.remove('bi-arrow-right-circle')
    icon.classList.add('bi-check2')
    let r = await c.transfer(nodeId,frActorId,toActorId,tokId,amount)
    await new Promise(resolve => setTimeout(resolve, 1000))
    let fr = JSON.stringify(r,null,2)
    await navigator.clipboard.writeText(fr)
    icon.classList.remove('bi-check2')
    icon.classList.add('bi-arrow-right-circle')
    redraw()
    jsonLog.push(["transfer",nodeId,frActorId,toActorId,tokId,amount])
  }
  transferBtn.addEventListener("click",transfer)
}

const clickAPIButton = async (evt) => {
  objectsHTML = spa.innerHTML
  const tgt = evt.target.closest(".api-button")
  const apiId = tgt.dataset.apiId
  const fnName = tgt.dataset.fn
  const nodeId = tgt.dataset.nodeId
  const apiName = tgt.dataset.apiName
  const t = "tuple"
  let val = prompt(`Enter Value for: ${apiName}`,"");
  if (val === null || val === "") {
    console.log("User cancelled the prompt.");
  } else {
    let v = JSON.stringify(JSON.parse(val))
    c.apiCall(apiId,nodeId,v,t)
    jsonLog.push(["apiCall",apiId,nodeId,v,t])
  }
}

const clickViewButton = async (evt) => {
  objectsHTML = spa.innerHTML
  const tgt = evt.target.closest(".view-button")
  const nodeId = tgt.dataset.nodeId
  const viewName = tgt.dataset.viewName
  const viewId = tgt.dataset.viewId
  const viewVar = tgt.dataset.viewVar
  const viewTag = tgt.dataset.viewTag
  const viewContents = tgt.dataset.viewContents
  let val = prompt(`Enter Value for: ${viewName}`,"");
  let v = -1
  let t = "number"
  if (val === null || val === "") {
    console.log("User cancelled the prompt.");
  } else {
    v = JSON.stringify(JSON.parse(val))
    type = "tuple"
  }
  let r = await c.viewCall(viewId,nodeId,v,t)
  let rr = JSON.stringify(r)
  alert(rr)
  jsonLog.push(["viewCall",viewId,nodeId,v,t])

}

//#### event handler which loads the Objects-Details View
const renderObjectDetails = async (evt) => {
  objectsHTML = spa.innerHTML
  const tgt = evt.target.closest(".object-button")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const actorSet = tgt.dataset.actorSet
  const apiSet = tgt.dataset.apiSet
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
  const act = await c.getActions(nodeId,actorId)
  let disableActorDets = ``
  if (!act) {
    disableActorDets = `disabled`
  }

  dets = dets + `
    <button type="button"
    ${disableActorDets}
    class="list-group-item list-group-item-action
    object-button status-panel"
    data-actor-id="${actorId}"
    data-node-id="${nodeId}"
    data-actor-set='${actorSet}'
    data-api-set='${apiSet}'
    data-who="${who}">
    <div class="badge bg-secondary">Status</div>
    <div> ${status} </div>
    </button> `
  for (const [k,v] of Object.entries(ledger[actorId])) {
    dets = dets + `
      <button type="button"
      disabled
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
      disabled
      class="list-group-item list-group-item-action object-button no-op"
      >
      <div class="badge bg-secondary">${typing.slice(2)}</div>
      <div> ${varName} := ${varValue} </div>
      </button> `
  }
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects"><span class="omph">Objects (${nodeId})</span></a></li>
      <li class="breadcrumb-item active" aria-current="page"><span class="omph">Details (${who})</span></li>
    </ol>
  </nav>
  <ul class="list-group list-group-flush">
    ${dets}
  </ul>
  `
  bindObjDetailsActionsEvents()
  setupReturnToObjects()
}

// helper to bind events for the Objects-Details View
const bindObjDetailsActionsEvents = () => {
  // currently we only allow clicks on "Program Status"
  const statusPanel = document.querySelectorAll(".status-panel")
  statusPanel.forEach((item, i) => {
    item.addEventListener("click",detailActions)
  });
  // const noOps = document.querySelectorAll(".no-op")
  // noOps.forEach((item, i) => {
  //   item.addEventListener("click",noOpHandler)
  // });
}

// helper to backtrack to the Objects View from the Objects-Details View
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

// helper to backtrack to the Details View from the Actions View
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

//#### event handler which loads the Details-Actions View
const detailActions = async (evt) => {
  detailsHTML = spa.innerHTML
  const tgt = evt.target.closest(".status-panel")
  const nodeId = tgt.dataset.nodeId
  const actorId = parseInt(tgt.dataset.actorId)
  const actorSet = tgt.dataset.actorSet
  const apiSet = tgt.dataset.apiSet
  const who = tgt.dataset.who
  const act = await c.getActions(nodeId,actorId)
  console.log(act)
  if (!act) {
    // noop();
    return false
  }
  let acts = ``
  acts = acts + renderAction(act,nodeId,actorId,who,actorSet,apiSet)
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects"><span class="omph">Objects (${nodeId})</span></a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-details"><span class="omph">Details (${who})</span></a></li>
      <li class="breadcrumb-item active" aria-current="page"><span class="omph">Actions</span></li>
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

// bind events for Actions view
const bindObjDetailsActionsResponseEvents = () => {
  const actionsPanel = document.querySelectorAll(".action-button")
  actionsPanel.forEach((item, i) => {
    item.addEventListener("click",respondToActions)
  });
}

//#### event handler which loads the Actions-Response View
const respondToActions = async (evt) => {
  actionsHTML = spa.innerHTML
  const tgt = evt.target.closest(".action-button")
  const nodeId = parseInt(tgt.dataset.nodeId)
  const actorId = parseInt(tgt.dataset.actorId)
  const actId = parseInt(tgt.dataset.actId)
  const who = tgt.dataset.who
  const actorSet = JSON.parse(tgt.dataset.actorSet)
  const tiebreakers = JSON.parse(tgt.dataset.tiebreakers)
  const act = tgt.dataset.act
  let actors = `<option value="">Unchanged</option>`
  for (const [k,v] of Object.entries(actorSet)) {
    actors = actors + `<option value="${k}">${v}</option>`
  }
  const respTempl = renderResponsePanel(nodeId,act,actors,actorId,actId,tiebreakers)
  spa.innerHTML = `
  <nav aria-label="breadcrumb">
    <ol class="breadcrumb">
      <li class="breadcrumb-item"><a href="#" id="return-to-objects"><span class="omph">Objects (${nodeId})</span></a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-details"><span class="omph">Details (${who})</span></a></li>
      <li class="breadcrumb-item"><a href="#" id="return-to-actions"><span class="omph">Actions</span></a></li>
      <li class="breadcrumb-item active" aria-current="page"><span class="omph">Response</span></li>
    </ol>
  </nav>
  ${respTempl[0]}
  <div>
    <button type="button" id="spa-res-button" class="btn btn-outline-light btn-sm">Respond</button>
  </div>
  `
  const spaRespondBtn = document.querySelector("#spa-res-button")
  spaRespondBtn.addEventListener("click",respTempl[1])

  setupReturnToObjects()
  setupReturnToObjectsDetails()
  setupReturnToActions()
}

// backtrack to Actions from Response view
const setupReturnToActions = () => {
  const actionsRetLink = document.querySelector("#return-to-actions")
  const backtrackToActions = () => {
    if (actionsHTML) {
      spa.innerHTML = actionsHTML
      actionsHTML = null;
      bindObjDetailsActionsResponseEvents();
      setupReturnToObjects()
      setupReturnToObjectsDetails()
    }
  }
  actionsRetLink.addEventListener("click",backtrackToActions)
}

// this is a helper method for the Response View
// based on the response type, this function returns an array of 2 elements
// 1ˢᵗ: the html for the view
// 2ⁿᵈ: the event handler for the view
const renderResponsePanel = (nodeId,act,actors,actorId,actId,tiebreakers) => {
  switch (act) {
    // case 'A_Interact':
    case 'A_TieBreak':
      let tbOpts = ``
      for (const [k,v] of Object.entries(tiebreakers)) {
        tbOpts = tbOpts + `<option value="${k}">${v}</option>`
      }
      const respondSpaTieBreak = async () => {
        let tiebreaker = document.querySelector("#tiebreakers-spa-select").value;
        tiebreaker = tiebreaker.split(":")
        let type = "number"
        let tiebreakerId = null
        if (tiebreaker[1] === "actor") {
          tiebreakerId = parseInt(tiebreaker[0])
        } else {
          type = "data"
          tiebreakerId = JSON.stringify({
            "tag":"V_Data",
            "contents":[
              "api",
              {
                "tag":"V_UInt",
                "contents": parseInt(tiebreaker[0])
              }
            ]
          })
        }
        let r = await c.respondWithVal(nodeId,actId,tiebreakerId,actorId,type)
        redraw()
        jsonLog.push(["respondWithVal",nodeId,actId,tiebreakerId,actorId,type])
      }
      return [
        `
          <div>
          <select name="tiebreakers" id="tiebreakers-spa-select">
          ${tbOpts}
          </select>
          </div>
        `,
        respondSpaTieBreak
      ]
    // case 'A_Remote':
    case 'A_Receive':
      const respondSpaContest = async () => {
        let r = await c.respondWithVal(nodeId,actId,0,actorId)
        redraw()
        jsonLog.push(["respondWithVal",nodeId,actId,0,actorId])
      }
      return [
        ``,
        respondSpaContest
      ]
    case 'A_AdvanceTime':
    case 'A_AdvanceSeconds':
    case 'A_None':
    default:
      const respondSpaDefault = async () => {
        let v = document.querySelector("#spa-response").value
        let selectedActorId = document.querySelector("#actors-spa-select").value;
        let t = document.querySelector("#typing-spa-select").value;
        let aid = actorId
        if (selectedActorId) {
          aid = parseInt(selectedActorId)
        }
        if (t === "number") {
          v = parseInt(v)
        } else if (t === "tuple") {
          v = JSON.stringify(JSON.parse(v))
        }
        let r = await c.respondWithVal(nodeId,actId,v,aid,t)
        redraw()
        jsonLog.push(["respondWithVal",nodeId,actId,v,aid,t])
      }
      return [
        `  <div class="bordered d-flex justify-content-center pad-me extra-margin">
            <input type="text" id="spa-response" class="form-control form-control-sm" placeholder="Value">

            <div>
              <select name="actors" id="actors-spa-select">
              ${actors}
              </select>
              <select name="typing" id="typing-spa-select">
                <option value="number">Number</option>
                <option value="token">Token</option>
                <option value="string">String</option>
                <option value="contract">Contract</option>
                <option value="address">Address</option>
                <option value="boolean">Boolean</option>
                <option value="tuple">Tuple</option>
                <option value="object">Object</option>
                <option value="data">Data</option>
                <option value="struct">Struct</option>
              </select>
            </div>

          </div>`,
          respondSpaDefault
      ]

  }
}

// helper to help render Actions
// the actions come as JSON objects from Scotty Server
// https://hackage.haskell.org/package/scotty-0.12/docs/Web-Scotty.html

// the objects may not necessarily be organized in the most intuitive way
// usually there is at least:
// a "tag" field which describes the object
// a "contents" field which contains the data for the object

// it may help to look at the objects in an interactive dev environment
// such as Google Chrome Dev Tools etc.
// that's what i did when building this so far :) - CA

const renderAction = (actObj,nodeId,actorId,who,actorSet,apiSet) => {
  const act = actObj[1]
  const actId = actObj[0]
  let tiebreakers = {}
  let tbList = act.contents[1]
  if (act.tag === 'A_TieBreak') {
    for (const [k,v] of Object.entries(JSON.parse(actorSet))) {
      if (tbList.includes(v)) {
        tiebreakers[`${k}:actor`] = v
      }
    }
    for (const [k,v] of Object.entries(JSON.parse(apiSet))) {
      if (tbList.includes(v)) {
        tiebreakers[`${k}:api`] = v
      }
    }
  }
  const common = `
  <button type="button"
  class="list-group-item list-group-item-action action-button"
  data-act-id="${actId}"
  data-actor-id="${actorId}"
  data-node-id="${nodeId}"
  data-actor-set='${actorSet}'
  data-who="${who}"
  data-act="${act.tag}"
  data-tiebreakers='${JSON.stringify(tiebreakers)}'>
  <div class="badge bg-secondary">
  `
  switch (act.tag) {
    case 'A_Interact':
      let domain = `Null`
      if (act.contents[4].length !== 0) {
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
    case 'A_Remote':
      let domain2 = `Null`
      let range = `Null`

      if (act.contents[2][0] && act.contents[2][0].contents.length && (act.contents[2][0].contents.length !== 0)) {
        domain2 = act.contents[2][0].contents.map(x => `${x[0]} : ${x[1].tag.slice(2)}` ).join(' , ')
      } else if (act.contents[2][0]) {
        domain2 = `${act.contents[2][0].tag.slice(2)} (${act.contents[2][0].contents})`
      }

      if (act.contents[3][0] && act.contents[3][0].contents.length && (act.contents[3][0].contents.length !== 0)) {
        range = act.contents[3][0].contents.map(x => `${x[0]} : ${x[1].tag.slice(2)}` ).join(' , ')
      } else if (act.contents[3][0]) {
        range = `${act.contents[3][0].tag.slice(2)} (${act.contents[3][0].contents})`
      }
      return `
        ${common}
        ${act.tag.slice(2)}</div>
        <div> ${act.contents[1]} : ${domain2} &#8594; ${range} </div>
        </button> `
    case 'A_Receive':
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

// redraw the Cytoscape visualization
const redraw = async () => {
  let edges = await c.getEdges()
  let states = await c.getStates()
  let elements = []
  const singleColors = ['#0f4539','#2f3b22','#052d0a','#001f24']
  for (const [s, dets] of Object.entries(states)) {
    let displayLabel = dets[1].tag.slice(2)
    let actorStateID = dets[0]
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
  let cy = cytoscape({
    container: document.getElementById('cy'),
    elements: elements,
    style: [
      {
        selector: 'node',
        style: {
          'background-color': '#f6f6f6',
          'label': 'data(id)',
          'visibility': 'hidden',
          'shape': 'round-rectangle',
          'content': 'data(label)',
          'font-family': 'Inconsolata, monospace',
          'background-color': '#f6f6f6',
          'color': '#2e3440',
          // 'font-size': 10,
          'width': '80%',
          // 'height': '5%',
          'text-valign': 'center',
          'text-halign': 'center',
          'border-width': 1.5,
          'border-color': 'data(color)',
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
  let allNodes = cy.filter(function(element, i){
    return element.isNode();
  });
  cy.on('click', 'node', function(evt){
    allNodes.style({
      'background-color': '#f6f6f6',
      'color': '#2e3440',
    })
    evt.target.style({
      'background-color': '#2e3440',
      'color': '#f6f6f6',
    })
  });
  const eles = cy.filter(function(element, i){
    return true;
  });
  // citation: animation adapted from https://gist.github.com/maxkfranz/aedff159b0df05ccfaa5
  // and https://stackoverflow.com/questions/40096407/animate-building-a-graph-in-cytoscape-js
  const animateGraph = (nodes) => {
    let delay = 0;
    let size = nodes.length;
    let duration = (1000 / size);
    const visitedMap = {};

    if (size === 1) {
      // single "frame", cancel animation
      nodes[0].style('visibility', 'visible')
      return;
    }

    for (let index = 0; index < size; ++index){
      visitedMap[nodes[index].data('id')] = 0;
    }
    var nodesCopy = nodes.clone();

    for (let i = 0; i < nodes.length; ++i ){
      const cNode = nodes[i];
      const nextNodes = cNode.connectedEdges(
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
      for (let index = 0; index < nextNodes.length; ++index){
        const nNode = nextNodes[index];
        (function(currentNode, x, copyNode, nextNode){
          if(nextNode !== null && x !== 0 && visitedMap[nextNode.data('id')] < 1){
            ++visitedMap[nextNode.data('id')];
            console.log('currentNode: ' + currentNode.data('id')+ ', x: ' + x + ', nextNode: ' + nextNode.data('id') );
            move(nextNode,currentNode,copyNode,false)
          } else if (nextNode !== null && visitedMap[nextNode.data('id')] < 1){
            ++visitedMap[nextNode.data('id')];
            move(nextNode,currentNode,copyNode,true)
          }
          delay += duration;
          })(cNode, i, nodesCopy[i], nNode);
      }
    } // for
    function compareFn(a, b) {
      const aId = parseInt(a.data('id'));
      const bId = parseInt(b.data('id'));
      if (aId < bId) {
        return -1;
      }
      if (aId > bId) {
        return 1;
      }
      return 0;
    }
    let focusEles = allNodes.sort(compareFn).slice(-3)
    setTimeout(() => { cy.animate({
      fit: {
        eles: focusEles,
        padding: 120
      }
    }, {
      duration: 250
    }); }, 1000);

  }; // animateGraph
  animateGraph(eles)
}

redraw();

// event handler for clicking a state node in the Cytoscape visualization
const clickNode = async (evt) => {
  const nodeId = evt.target.id()
  const r = await c.getStateLocals(nodeId)
  let actors = []
  for (const [k,v] of Object.entries(r.l_locals)) {
    actors.push(k)
  }
  const atsList = await Promise.all(actors.map(async (actorId) => {
  	let x = await c.getLoc(nodeId,actorId)
    return x
  }));
  let ats = atsList
    .filter(at => at)
    .map(at => at.split(':')[1]);
  while (sheet.cssRules.length > 0) {
    sheet.deleteRule(sheet.cssRules.length - 1);
  }
  const singleColors = ['#0f4539','#2f3b22','#052d0a','#001f24']
  const multiColor = '#152d32'
  const dupls = ats.filter((e, index, arr) => arr.indexOf(e) !== index)
  const dups = [...new Set(dupls)]
  ats = [...new Set(ats)]
  const diff = ats.filter(x => !dups.includes(x));
  const singles = diff.map(at => `.hljs-ln-line[data-line-number="${at}"]`)
  const multiples = dups.map(at => `.hljs-ln-line[data-line-number="${at}"]`)

  singles.forEach((clss, i) => {
    sheet.insertRule(`${clss} {
      background-color: ${singleColors[i % singleColors.length]};
    }`);
  });

  multiples.forEach((clss, i) => {
    sheet.insertRule(`${clss} {
      background-color: ${multiColor};
    }`);
  });

  const at = ats.at(-1)
  if (at) {
    const poi = document.querySelector(`.hljs-ln-line[data-line-number="${at}"`);
    const topPos = poi.offsetTop;
    document.querySelector('.code-container').scrollTop = topPos;
  }
  renderObjects(nodeId)
}

// load a JSON simulation "script" (not to be confused with Reach script)
const loadScriptBtn = document.querySelector("#load-script")
const loadScript = async (evt) => {
  let icon = evt.target.querySelector('.bi')
  if (!icon) {
    icon = evt.target
  }
  icon.classList.remove('bi-play-circle')
  icon.classList.add('bi-check2')
  await new Promise(resolve => setTimeout(resolve, 1000))
  let j = await navigator.clipboard.readText()
  let script = JSON.parse(j)
  await c.interp(script)
  jsonLog = script
  redraw()
  icon.classList.remove('bi-check2')
  icon.classList.add('bi-play-circle')
}
loadScriptBtn.addEventListener("click",loadScript)

// save a JSON simulation "script"
const saveScriptBtn = document.querySelector("#save-script")
const saveScript = async (evt) => {
  let icon = evt.target.querySelector('.bi')
  if (!icon) {
    icon = evt.target
  }
  icon.classList.remove('bi-clipboard')
  icon.classList.add('bi-check2')
  await new Promise(resolve => setTimeout(resolve, 1000))
  let r = JSON.stringify(jsonLog,null,2)
  await navigator.clipboard.writeText(r)
  icon.classList.remove('bi-check2')
  icon.classList.add('bi-clipboard')
}
saveScriptBtn.addEventListener("click",saveScript)
