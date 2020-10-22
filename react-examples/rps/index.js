import React from 'react';
import ReactDOM from 'react-dom';
import AppViews from './views/AppViews';
import DeployerViews from './views/DeployerViews';
import AttacherViews from './views/AttacherViews';
import './index.css';
import * as backend from './build/index.main.mjs';
import * as reach from '@reach-sh/stdlib/ETH';

const {standardUnit} = reach;
const defaultFundAmt = '10';
const defaultWager = '3';
const defaultEscrow = '3';
const defaults = {defaultFundAmt, defaultWager, defaultEscrow};

function renderDOM() {
  ReactDOM.render(
    <React.StrictMode><App /></React.StrictMode>,
    document.getElementById('root')
  );
}

function renderView(parent, Views) {
  parent.state = parent.state || {};
  const {view, ContentView} = parent.state;
  const View = view === 'Wrapper'
    ? ContentView
    : Views[parent.state.view];
  if (!View) {
    // XXX deleteme
    alert(`missing ${view}`);
  }
  const Wrapper = Views['Wrapper'];
  const props = {...parent.props, ...parent.state, parent};
  const content = <View {...props} />;
  return <Wrapper {...{content}} />;
}

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {view: 'ConnectAccount', ...defaults};
  }
  async componentDidMount() {
    const acc = await reach.getDefaultAccount();
    const addr = await acc.networkAccount.getAddress();
    const balAtomic = await reach.balanceOf(acc)
    const bal = reach.formatCurrency(balAtomic, 4);
    this.setState({acc, addr, bal, defaultFundAmt, standardUnit});
    try {
      const faucet = await reach.getFaucet();
      this.setState({view: 'FundAccount', faucet});
    } catch (e) {
      this.setState({view: 'DeployerOrAttacher'})
    }
  }
  async fundAccount(fundAmount) {
    const {acc, faucet} = this.state;
    const fundAmountAtomic = reach.parseCurrency(fundAmount);
    await reach.transfer(faucet, acc, fundAmountAtomic);
    this.setState({view: 'DeployerOrAttacher'});
  }
  async skipFundAccount() {
    this.setState({view: 'DeployerOrAttacher'});
  }
  selectAttacher() {
    this.setState({view: 'Wrapper', ContentView: Attacher});
  }
  selectDeployer() {
    this.setState({view: 'Wrapper', ContentView: Deployer});
  }
  render() {
    return renderView(this, AppViews);
  }
}

class Player extends React.Component {
  playHand(hand) {
    this.state.resolveHandP(hand);
  }
  random() { return reach.hasRandom.random(); }
  async getHand() { // getHand: Fun([], Bytes)
    console.log('XXX: getHand');
    let resolveHandP = null;
    const handP = new Promise(f => { resolveHandP = f; });
    this.setState({view: 'GetHand', playable: true, resolveHandP});
    const hand = await handP;
    this.setState({view: 'WaitingForResults', hand});
    return hand;
  }
  partnerIs(partnerAddr) { // partnerIs: Fun([Address], Null)
    console.log('XXX: partnerIs');
    console.log(partnerAddr);
    this.setState({view: 'WaitingForTurn', partnerAddr});
  }
  endsWith(bytes) { // endsWith: Fun([Bytes], Null)
    console.log('XXX: endsWith');
    const result = bytes; // no hexToString?
    console.log(result);
    this.setState({view: 'Done', result});
  }
}

class Deployer extends Player {
  constructor(props) {
    super(props);
    this.state = {view: 'SetWager'};
  }
  setWager(wager) {
    this.setState({view: 'SetEscrow', wager});
  }
  setEscrow(escrow) {
    const wagerAtomic = reach.parseCurrency(this.state.wager);
    const escrowAtomic = reach.parseCurrency(escrow);
    const totalAtomic = reach.add(wagerAtomic, escrowAtomic);
    const total = reach.formatCurrency(totalAtomic, 4);
    this.setState({view: 'Deploy', escrow, total, wagerAtomic, escrowAtomic});
  }
  async deploy() {
    const {wagerAtomic, escrowAtomic} = this.state;
    const ctc = this.props.acc.deploy(backend);
    this.setState({view: 'Deploying', ctc});
    backend.A(reach, ctc, this); // Note: no await
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({view: 'WaitingForAttacher', ctcInfoStr});
  }
  getParams() { // getParams: Fun([], Tuple(UInt, UInt))
    console.log('XXX getParams');
    const {wagerAtomic, escrowAtomic} = this.state;
    return [wagerAtomic, escrowAtomic];
  }
  commits() { // commits: Fun([], Null)
    console.log('XXX commits');
  }
  reveals(bytes) { // reveals: Fun([Bytes], Null)
    console.log('XXX reveals');
    console.log(bytes);
    // console.log(reach.hexToString(bytes));
  }
  render() {
    return renderView(this, DeployerViews);
  }
}

class Attacher extends Player {
  constructor(props) {
    super(props);
    this.state = {view: 'Attach'};
  }
  attach(ctcInfoStr) {
    const ctcInfo = JSON.parse(ctcInfoStr);
    const {acc} = this.props;
    const ctc = acc.attach(backend, ctcInfo);
    this.setState({view: 'Attaching'});
    backend.B(reach, ctc, this);
  }
  termsAccepted() {
    this.state.resolveAcceptedP();
  }
  // acceptParams: Fun([UInt, UInt], Null)
  async acceptParams(wagerAtomic, escrowAtomic) {
    console.log('XXX acceptParams');
    const wager = reach.formatCurrency(wagerAtomic, 4);
    const escrow = reach.formatCurrency(escrowAtomic, 4);
    console.log({wager, escrow});

    let resolveAcceptedP = null;
    const acceptedP = new Promise(f => { resolveAcceptedP = f });
    this.setState({view: 'AcceptTerms', wager, escrow, resolveAcceptedP});
    await acceptedP;
  }
  // shows: Fun([], Null)
  shows() { console.log('XXX shows'); }
  render() {
    return renderView(this, AttacherViews);
  }
}

renderDOM();
