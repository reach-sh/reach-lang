/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * @format
 * @flow strict-local
 */

import React from 'react';
import AppViews from './src/AppViews';
import DeployerViews from './src/DeployerViews';
import AttacherViews from './src/AttacherViews';
import { renderView } from './src/render';
import * as backend from './build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
import { Platform } from 'react-native';

const reach = loadStdlib(process.env);

const handToInt = { ROCK: 0, PAPER: 1, SCISSORS: 2 };
const intToOutcome = ['Bob wins!', 'Draw!', 'Alice wins!'];
const { standardUnit } = reach;
const defaults = { defaultFundAmt: '10', defaultWager: '0.005', standardUnit };

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = { view: 'ConnectAccount', ...defaults };
    const init = async () => {
      await reach.setProviderByName('TestNet');
    }
    init();
  }
  async componentDidMount() {
    // const acc = await reach.getDefaultAccount();

    if (await reach.canFundFromFaucet()) {
      this.setState({ view: 'FundAccount' });
    } else {
      this.setState({ view: 'DeployerOrAttacher' });
    }
  }

  async setAccount(isDeployer = true) {
    const phrase = isDeployer ?
      "humor sting race bonus unit arctic speak fine wood double hip crouch"
      :
      "oppose settle table giggle flush seven addict wrap pull jelly payment purchase";
    const acc = await reach.newAccountFromMnemonic(phrase);
    const balAtomic = await reach.balanceOf(acc);
    const bal = reach.formatCurrency(balAtomic, 4);
    this.setState({ acc, bal });
  }

  async fundAccount(fundAmount) {
    await reach.fundFromFaucet(this.state.acc, reach.parseCurrency(fundAmount));
    this.setState({ view: 'DeployerOrAttacher' });
  }
  async skipFundAccount() {
    this.setState({ view: 'DeployerOrAttacher' });
  }
  async selectAttacher() {
    await this.setAccount(true)
    this.setState({ view: 'Wrapper', ContentView: Attacher });
  }
  async selectDeployer() {
    await this.setAccount(false)
    this.setState({ view: 'Wrapper', ContentView: Deployer });
  }
  render() {
    return renderView(this, AppViews);
  }
}

class Player extends React.Component {
  random() {
    return reach.hasRandom.random();
  }
  async getHand() {
    // Fun([], UInt)
    const hand = await new Promise(resolveHandP => {
      this.setState({ view: 'GetHand', playable: true, resolveHandP });
    });
    this.setState({ view: 'WaitingForResults', hand });
    return handToInt[hand];
  }
  seeOutcome(i) {
    this.setState({ view: 'Done', outcome: intToOutcome[i] });
  }
  informTimeout() {
    this.setState({ view: 'Timeout' });
  }
  playHand(hand) {
    this.state.resolveHandP(hand);
  }
}

class Deployer extends Player {
  constructor(props) {
    super(props);
    this.state = { view: 'SetWager' };
  }
  setWager(wager) {
    this.setState({ view: 'Deploy', wager });
  }
  async deploy() {
    const ctc = this.props.acc.deploy(backend);
    this.setState({ view: 'Deploying', ctc });
    this.wager = reach.parseCurrency(this.state.wager); // UInt
    this.deadline = { ETH: 10, ALGO: 100, CFX: 1000 }[reach.connector]; // UInt
    backend.Alice(ctc, this);
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({ view: 'WaitingForAttacher', ctcInfoStr });
  }
  render() {
    return renderView(this, DeployerViews);
  }
}
class Attacher extends Player {
  constructor(props) {
    super(props);
    this.state = { view: 'Attach' };
  }
  attach(ctcInfoStr) {
    const ctc = this.props.acc.attach(backend, JSON.parse(ctcInfoStr));
    this.setState({ view: 'Attaching' });
    backend.Bob(ctc, this);
  }
  async acceptWager(wagerAtomic) {
    // Fun([UInt], Null)
    const wager = reach.formatCurrency(wagerAtomic, 4);
    return await new Promise(resolveAcceptedP => {
      this.setState({ view: 'AcceptTerms', wager, resolveAcceptedP });
    });
  }
  termsAccepted() {
    this.state.resolveAcceptedP();
    this.setState({ view: 'WaitingForTurn' });
  }
  render() {
    return renderView(this, AttacherViews);
  }
}

export default App;
