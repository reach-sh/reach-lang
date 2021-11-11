/**
 * Sample React Native App
 * https://github.com/facebook/react-native
 *
 * @format
 * @flow strict-local
 */

import React from 'react';
import * as AppViews from './src/AppViews';
import * as AliceViews from './src/AliceViews';
import * as BobViews from './src/BobViews';
import * as backend from './build/index.main.mjs';
import {loadStdlib} from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);
const {standardUnit} = reach;
const defaultFundAmtStandard = '10';
const defaultInfo = 'the cake is a lie';
const defaultRequestStandard = '0.005';

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'ConnectAccount'}
    const init = async () => {
      reach.setProviderByName('TestNet');
    }
    init();
  }
  async componentDidMount() { // from mode: ConnectAccount
    // const acc = await reach.getDefaultAccount();
    const phrase = Platform.OS === 'ios'?
      "humor sting race bonus unit arctic speak fine wood double hip crouch" 
      : 
      "oppose settle table giggle flush seven addict wrap pull jelly payment purchase";
    const acc = await reach.newAccountFromMnemonic(phrase);
    const addr = reach.formatAddress(await acc.getAddress());
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
    const ctc = this.props.acc.contract(backend);
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

export default App;
