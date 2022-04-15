import * as c from '@reach-sh/simulator-client';

// nodejs compatible import
// const c = await import('@reach-sh/simulator-client');

// TODO: typescript

const consensusID = -1
const nwToken = -1

Object.filter = (obj, predicate) =>
  Object.fromEntries(Object.entries(obj).filter(predicate));


class Scenario {
  constructor() {
    this.stateID = 0
    this.participants = [];
    this.consensus = new Consensus(new Account(-1),this);
    this.apis = [];
    this.views = [];
  }

  async init() {
    // reset server
    await c.resetServer()
    // load the Reach program
    const rsh = await c.load()
    // initialize the program for the Consensus
    await c.init()
    const apis = await c.getAPIs()
    const views = await c.getViews(this.stateID)
    const l = await c.getStateLocals(this.stateID)

    // setup parts
    for (const [k,v] of Object.entries(l.l_locals)) {
      const who = v.l_who ? v.l_who : 'Consensus'
      const acc = new Account(v.l_acct)
      if (who) {
        const p = new Participant(k,acc,who,this)
        this.participants.push(p)
      }
    }

    // setup apis
    for (const [k,v] of Object.entries(apis)) {
      const who = v.a_name
      const a = new API(k,who,this)
      this.apis.push(a)
    }

    // setup views
    for (const [k,v] of Object.entries(views)) {
      const who = v.a_name
      const vari = v.v_var
      const tag = v.v_ty.tag
      const contents = v.v_ty.contents
      const v = new View(k,who,vari,tag,contents,this)
      this.views.push(v)
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
    const l = await c.getStateLocals(this.stateID)
    if (l.l_curr_actor_id === consensusID) {
      return this.consensus
    } else {
      return this.participants[l.l_curr_actor_id]
    }
  }

  async newAccount() {
    return await c.newAccount(this.stateID);
  }

  async newToken() {
    return await c.newToken(this.stateID);
  }
}

class Participant {
  constructor(id,account,name,scene) {
    this.id = id;
    this.account = account;
    this.name = name;
    this.scene = scene;
  }

  async init(liv={},accID="") {
    const r = await c.initFor(this.scene.stateID,this.id,JSON.stringify(liv),accID)
    ++this.scene.stateID;
    return r;
  }

  async history() {
    const sg = await c.getStateGraph();
    const filtered = Object.filter(sg, ([id, state]) =>
      state[1].contents.l_curr_actor_id === this.id
    );
    return filtered;
  }

  async getNextAction() {
    const act = await c.getActions(this.scene.stateID,this.id)
    return new Action(act[0],act[1].tag,this,this.scene);
  }

  async getStore() {
    const l = await c.getStateLocals(this.scene.stateID)
    return l.l_locals[this.id].l_store
  }

  async getPhase() {
    const l = await c.getStateLocals(this.scene.stateID)
    return l.l_locals[this.id].l_phase
  }

  async getStatus() {
    const l = await c.getStateLocals(this.scene.stateID)
    switch (l.l_locals[this.id].l_ks) {
      case 'PS_Suspend':
        return 'Running';
      case 'PS_Done':
        return 'Done';
    }
  }

  async getWallet() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_ledger[this.account.id]
  }

}

// TODO: polymorphism, Actor interface
class Consensus {
  constructor(account,scene) {
    this.account = account;
    this.id = consensusID
    this.scene = scene;
  }

  async transfer(s,fr,to,tok,amt) {
    const frID = fr.id
    const toID = to.id
    const tokID = tok.id
    return await c.tranfer(s,frID,toID,tokID,amt);
  }

  async history() {
    const sg = await c.getStateGraph();
    const filtered = Object.filter(sg, ([id, state]) =>
      state[1].contents.l_curr_actor_id === this.id
    );
    return filtered;
  }

  async getNextAction() {
    const act = await c.getActions(this.scene.stateID,this.id)
    return new Action(act[0],act[1].tag,this,this.scene);
  }

  async getLedger() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_ledger
  }

  async getLinearState() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_linstate
  }

  async getNetworkTime() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_nwtime
  }

  async getNetworkSeconds() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_nwsecs
  }

  async getLog() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_messages
  }

  async getStore() {
    const l = await c.getStateLocals(this.scene.stateID)
    return l.l_locals[this.id].l_store
  }

  async getPhase() {
    const l = await c.getStateLocals(this.scene.stateID)
    return l.l_locals[this.id].l_phase
  }

  async getStatus() {
    const l = await c.getStateLocals(this.scene.stateID)
    switch (l.l_locals[this.id].l_ks) {
      case 'PS_Suspend':
        return 'Running';
      case 'PS_Done':
        return 'Done';
    }
  }

  async getWallet() {
    const g = await c.getStateGlobals(this.scene.stateID)
    return g.e_ledger[this.account.id]
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
    const r = await c.respondWithVal(this.scene.stateID,this.id,resp,this.owner.id,ty)
    ++this.scene.stateID;
    return r;
  }

}

class Account {
  constructor(id) {
    this.id = id;
  }

  async getWallet() {
    const g = await c.getStateGlobals(this.stateID)
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
    return await c.viewCall(this.id,scene.stateID,v,t)
  }
}

class API {
  constructor(id,name,scene) {
    this.id = id;
    this.name = name;
    this.scene = scene;
  }

  async call(v,t) {
    return await c.apiCall(this.id,scene.stateID,v,t);
  }
}

export {
  Scenario,
  Participant,
  Consensus,
  Action,
  Account,
  Token,
  View,
  API
};
