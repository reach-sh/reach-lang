// import * as c from '@reach-sh/simulator-client';
var c = await import('@reach-sh/simulator-client');

class Scenario {
  constructor() {
    this.parts = [];
    this.cons = [];
    this.apis = [];
    this.views = [];
  }
  init() {
    return this.id;
  }
  pingServer() {
    return c.ping();
  }
  reset() {
    return this.id;
  }
  programHistory() {
    return this.id;
  }
  getCurrentActor() {
    return this.id;
  }
  newAccount() {
    return this.id;
  }
  newToken() {
    return this.id;
  }
}

class Participant {
  constructor(id,account) {
    this.id = id;
    this.account = account;
  }
  history() {
    return this.id;
  }
  getNextInteract() {
    return this.id;
  }
  getStore() {
    return this.id;
  }
  getPhase() {
    return this.id;
  }
}

class Consensus {
  constructor(account) {
    this.account = account;
  }
  transfer() {
    return this.id;
  }
  history() {
    return this.id;
  }
  getNextTiebreak() {
    return this.id;
  }
  getNextRemote() {
    return this.id;
  }
  getLedger() {
    return this.id;
  }
  getLinearState() {
    return this.id;
  }
  getNetworkTime() {
    return this.id;
  }
  getNetworkSeconds() {
    return this.id;
  }
  getLog() {
    return this.id;
  }
  getPhase() {
    return this.id;
  }
}

class Action {
  constructor(id,name,owner) {
    this.id = id;
    this.name = name;
    this.owner = owner;
  }
  resolve() {
    return this.id;
  }
}

class Account {
  constructor(id) {
    this.id = id;
  }
  getWallet() {
    return this.id;
  }
}

class Token {
  constructor(id) {
    this.id = id;
  }
}

class View {
  constructor(id) {
    this.id = id;
  }
  call() {
    return this.id;
  }
}

class API {
  constructor(id) {
    this.id = id;
  }
  call() {
    return this.id;
  }
}
