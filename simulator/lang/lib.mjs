import * as c from '@reach-sh/simulator-client';
import * as v8 from 'v8';

// nodejs compatible import
// const c = await import('@reach-sh/simulator-client');

// TODO: typescript

const consensusID = -1
const nwToken = -1


const structuredClone = obj => {
  return v8.deserialize(v8.serialize(obj));
};

Object.filter = (obj, predicate) =>
  Object.fromEntries(Object.entries(obj).filter(predicate));


class State {
  constructor() {
    this.id = 0
  }

  next() {
    return ++this.id;
  }
}

class Scenario {
  constructor() {
    this.state = new State();
    this.participants = {};
    this.consensus = new Consensus(new Account(-1),this);
    this.apis = {};
    this.views = {};
  }

  async init() {
    // reset server
    await c.resetServer()
    // load the Reach program
    const rsh = await c.load()
    // initialize the program for the Consensus
    await c.init()
    const apis = await c.getAPIs()
    const views = await c.getViews(this.state.id)
    const l = await c.getStateLocals(this.state.id)

    // setup parts
    for (const [k,v] of Object.entries(l.l_locals)) {
      const who = v.l_who
      const acc = new Account(v.l_acct)
      if (who) {
        const p = new Participant(k,acc,who,this)
        this.participants[who] = p
      }
    }

    // setup apis
    for (const [k,v] of Object.entries(apis)) {
      const who = v.a_name
      const a = new API(k,who,this)
      this.apis[who] = a
    }

    // setup views
    for (const [k,v] of Object.entries(views)) {
      const who = v.a_name
      const vari = v.v_var
      const tag = v.v_ty.tag
      const contents = v.v_ty.contents
      const v = new View(k,who,vari,tag,contents,this)
      this.views[who] = v
    }
  }

  async pingServer() {
    return await c.ping();
  }

  async reset() {
    return await c.resetServer();
  }

  async programHistory() {
    return await c.getStates();
  }

  async getCurrentActor() {
    const l = await c.getStateLocals(this.state.id)
    if (l.l_curr_actor_id === consensusID) {
      return this.consensus
    } else {
      return this.participants[l.l_curr_actor_id]
    }
  }

  async newTestAccount() {
    return await c.newAccount(this.state.id);
  }

  async launchToken() {
    return await c.newToken(this.state.id);
  }
}

class FunctionalScenario extends Scenario {
  constructor() {
    super();
  }

  next() {
    const next = structuredClone(this);
    next.state.next();
    return next;
  }
}

class ImperativeScenario extends Scenario {
  constructor() {
    super();
  }

  next() {
    this.state.next();
    return this;
  }
}

class Actor {

  async getNextAction() {
    const act = await c.getActions(this.scene.state.id,this.id)
    return new Action(act[0],act[1].tag,this,this.scene);
  }

  async getStore() {
    const l = await c.getStateLocals(this.scene.state.id)
    return l.l_locals[this.id].l_store
  }

  async getWallet() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_ledger[this.account.id]
  }


  async getPhase() {
    const l = await c.getStateLocals(this.scene.state.id)
    return l.l_locals[this.id].l_phase
  }

  async getStatus() {
    const l = await c.getStateLocals(this.scene.state.id)
    switch (l.l_locals[this.id].l_ks) {
      case 'PS_Suspend':
        return 'Running';
      case 'PS_Done':
        return 'Done';
    }
  }

  async history() {
    const sg = await c.getStateGraph();
    const filtered = Object.filter(sg, ([id, state]) =>
      state[1].contents.l_curr_actor_id === this.id
    );
    return filtered;
  }

}

class Participant extends Actor {
  constructor(id,account,name,scene) {
    super();
    this.id = id;
    this.account = account;
    this.name = name;
    this.scene = scene;
  }

  async init(liv={},accID="") {
    const r = await c.initFor(this.scene.state.id,this.id,JSON.stringify(liv),accID)
    console.log(r);
    return this.scene.next();
  }

}

class Consensus extends Actor {
  constructor(account,scene) {
    super();
    this.account = account;
    this.id = consensusID
    this.scene = scene;
  }

  async transfer(s,fr,to,tok,amt) {
    const frID = fr.id
    const toID = to.id
    const tokID = tok.id
    return await c.transfer(s,frID,toID,tokID,amt);
  }

  async getLedger() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_ledger
  }

  async getMapState() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_linstate
  }

  async getNetworkTime() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_nwtime
  }

  async getNetworkSeconds() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_nwsecs
  }

  async getLog() {
    const g = await c.getStateGlobals(this.scene.state.id)
    return g.e_messages
  }

}

class Action {
  constructor(id,name,owner,scene) {
    this.id = id;
    this.name = name;
    this.owner = owner;
    this.scene = scene;
  }

  async resolve(resp,ty="number") {
    const r = await c.respondWithVal(this.scene.state.id,this.id,resp,this.owner.id,ty)
    console.log(r);
    return this.scene.next();
  }

}

class Account {
  constructor(id) {
    this.id = id;
  }

  async getWallet() {
    const g = await c.getStateGlobals(this.state.id)
    return g.e_ledger[this.id]
  }
}

class Token {
  constructor(id) {
    this.id = id;
  }
}

class View {
  constructor(id,name,vari,tag,contents,scene) {
    this.id = id;
    this.name = name;
    this.vari = vari;
    this.tag = tag;
    this.contents = contents;
    this.scene = scene;
  }

  async call(v,t) {
    return await c.viewCall(this.id,scene.state.id,v,t)
  }
}

class API {
  constructor(id,name,scene) {
    this.id = id;
    this.name = name;
    this.scene = scene;
  }

  async call(v,t) {
    return await c.apiCall(this.id,scene.state.id,v,t);
  }
}

export {
  Scenario,
  ImperativeScenario,
  FunctionalScenario,
  Participant,
  Consensus,
  Action,
  Account,
  Token,
  View,
  API
};
