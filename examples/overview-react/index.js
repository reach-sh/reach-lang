import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import * as AppViews from './views/AppViews';
import * as AliceViews from './views/AliceViews';
import * as BobViews from './views/BobViews';
import * as backend from './build/index.main.mjs';
import * as reach from '@reach-sh/stdlib/ETH';

const {standardUnit} = reach;
const defaultFundAmtStandard = '10';
const defaultInfo = 'the cake is a lie';
const defaultRequestStandard = '0.5';

function renderDOM() {
  ReactDOM.render(
    <React.StrictMode><App /></React.StrictMode>,
    document.getElementById('root')
  );
}

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'ConnectAccount'}
  }
  async componentDidMount() { // from mode: ConnectAccount
    const acc = await reach.getDefaultAccount();
    const addr = await acc.getAddress();
    const balAtomic = await reach.balanceOf(acc);
    const bal = reach.formatCurrency(balAtomic, 4);
    try {
      const faucet = await reach.getFaucet();
      this.setState({mode: 'FundAccount', acc, addr, bal, faucet});
    } catch (e) {
      this.setState({mode: 'SelectRole', acc, addr, bal});
    }
  }
  fundAccount(fundAmountStandard) { // from mode: FundAccount
    const {faucet, acc} = this.state;
    const amountAtomic = reach.parseCurrency(fundAmountStandard || defaultFundAmtStandard);
    reach.transfer(faucet, acc, amountAtomic);
    this.setState({mode: 'SelectRole'});
  }
  skipFundAccount() { this.setState({mode: 'SelectRole'}); } // from mode: FundAccount
  selectRole(role) { this.setState({mode: 'RunRole', role}); } // from mode: SelectRole
  selectBob() { this.selectRole(<Bob acc={this.state.acc} />); }
  selectAlice() { this.selectRole(<Alice acc={this.state.acc} />); }
  render() {
    const {mode, addr, bal, role} = this.state;
    const parent = this;
    let app = null;
    if (mode === 'ConnectAccount') {
      app = <AppViews.ConnectAccount />
    } else if (mode === 'FundAccount') {
      app = <AppViews.FundAccount {...{parent, addr, bal, standardUnit, defaultFundAmtStandard}} />
    } else if (mode === 'SelectRole') {
      app = <AppViews.SelectRole {...{parent}} />
    } else { // 'RunRole'
      app = role;
    }
    return <AppViews.Wrapper {...{app}} />;
  }
}

class Alice extends React.Component {
  constructor(props) {
    super(props);
    this.state = { mode: 'Deploy'};
  }
  async deploy() { // from mode: Deploy
    const ctc = this.props.acc.deploy(backend);
    this.setState({mode: 'EnterInfo', ctc});
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({ctcInfoStr});
  }
  enterInfo(info) { this.setState({mode: 'EnterRequest', info}); } // from mode: EnterInfo
  enterRequest(requestStandard) { this.setState({mode: 'RunBackend', requestStandard}); } // from mode: EnterRequest
  async runBackend() { // from mode: RunBackend
    const {ctc, requestStandard, info} = this.state;
    this.setState({mode: 'BackendRunning'});
    const request = reach.parseCurrency(requestStandard);
    await backend.Alice(ctc, {request, info});
    this.setState({mode: 'BackendRan'});
  }
  render() {
    let alice = null;
    const parent = this;
    const {mode, ctcInfoStr, requestStandard, info} = this.state;
    if (mode === 'Deploy') {
      alice = <AliceViews.Deploy {...{parent}} />;
    } else if (mode === 'EnterInfo') {
      alice = <AliceViews.EnterInfo {...{parent, defaultInfo}} />;
    } else if (mode === 'EnterRequest') {
      alice = <AliceViews.EnterRequest {...{parent, standardUnit, defaultRequestStandard}} />;
    } else if (mode === 'RunBackend') {
      alice = <AliceViews.RunBackend {...{parent, info, requestStandard, standardUnit}} />;
    } else if (mode === 'BackendRunning') {
      alice = <AliceViews.BackendRunning {...{ctcInfoStr}} />;
    } else { // 'BackendRan'
      alice = <AliceViews.BackendRan />;
    }
    return <AliceViews.AliceWrapper {...{alice}} />
  }
}

class Bob extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'RunBackend'};
  }
  async runBackend(ctcInfoStr) { // from mode: RunBackend
    const ctcInfo = JSON.parse(ctcInfoStr);
    const ctc = this.props.acc.attach(backend, ctcInfo);
    this.setState({mode: 'ApproveRequest'});
    const interact = {
      want: (request) => this.setState({mode: 'DisplayInfo', requestStandard: reach.formatCurrency(request, 4)}),
      got: (info) => this.setState({info}),
    };
    await backend.Bob(ctc, interact);
  }
  render() {
    let bob = null;
    const parent = this;
    const {mode, requestStandard, info} = this.state;
    if (mode === 'RunBackend') {
      bob = <BobViews.RunBackend {...{parent}} />
    } else if (mode === 'ApproveRequest') {
      bob = <BobViews.ApproveRequest {...{requestStandard}} />;
    } else { // 'DisplayInfo'
      bob = <BobViews.DisplayInfo {...{info}} />
    }
    return <BobViews.BobWrapper {...{bob}} />;
  }
}

renderDOM();
