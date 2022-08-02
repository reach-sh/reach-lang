import React from 'react';
import AppViews from './views/AppViews';
import DeployerViews from './views/DeployerViews';
import AttacherViews from './views/AttacherViews';
import {renderDOM, renderView} from './views/render';
import './index.css';
import * as backend from './build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);

const amt = 1

const {standardUnit} = reach;
const defaults = {defaultFundAmt: '10', defaultPrice: '3', standardUnit};

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {view: 'ConnectAccount', ...defaults};
  }
  async firstNFT () {
	await stdlib.launchToken(accMaker, "nft1", "FRST", {supply: 1 });
}
  async makerAccept () {
	await accMaker.tokenAccept(firstNFT.id);
}
  async buyerAccept () {
	await accBuyer.tokenAccept(firstNFT.id);
}
  
  async componentDidMount() {
    const acc = await reach.getDefaultAccount();
    const balAtomic = await reach.balanceOf(acc);
    const bal = reach.formatCurrency(balAtomic, 4);
    this.setState({acc, bal});
    if (await reach.canFundFromFaucet()) {
      this.setState({view: 'FundAccount'});
    } else {
      this.setState({view: 'DeployerOrAttacher'});
    }
  }
  async fundAccount(fundAmount) {
    await reach.fundFromFaucet(this.state.acc, reach.parseCurrency(fundAmount));
    this.setState({view: 'DeployerOrAttacher'});
  }
  async skipFundAccount() { this.setState({view: 'DeployerOrAttacher'}); }
  selectAttacher() { this.setState({view: 'Wrapper', ContentView: Attacher}); }
  selectDeployer() { this.setState({view: 'Wrapper', ContentView: Deployer}); }
  render() { return renderView(this, AppViews); }
}

class Maker extends React.Component {
  async nftID() { // Fun([], UInt)
    const firstNFT = await new Promise(createfirstNFT => {
      this.setState({view: 'nftID'});
    });
    this.setState({view: 'WaitingForResults', firstNFT});
    return firstNFT.id;
  }
}

class Deployer extends Maker {
  constructor(props) {
    super(props);
    this.state = {view: 'SetWager'};
  }
  setPrice(price) { this.setState({view: 'Deploy', price}); }
  async deploy() {
    const ctc = this.props.acc.contract(backend);
    this.setState({view: 'Deploying', ctc});
    this.price = reach.parseCurrency(this.state.price); // UInt
    backend.Maker(ctc, this);
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({view: 'WaitingForAttacher', ctcInfoStr});
  }
  render() { return renderView(this, DeployerViews); }
}
class Attacher extends Buyer {
  constructor(props) {
    super(props);
    this.state = {view: 'Attach'};
  }
  attach(ctcInfoStr) {
    const ctc = this.props.acc.contract(backend, JSON.parse(ctcInfoStr));
    this.setState({view: 'Attaching'});
    backend.Buyer(ctc, this);
  }
  async acceptPrice(priceAtomic) { // Fun([UInt], Null)
    const price = reach.formatCurrency(priceAtomic, 4);
    return await new Promise(resolveAcceptedP => {
      this.setState({view: 'AcceptTerms', price, resolveAcceptedP});
    });
  }
  termsAccepted() {
    this.state.resolveAcceptedP();
    this.setState({view: 'WaitingForTurn'});
  }
  render() { return renderView(this, AttacherViews); }
}

renderDOM(<App />);
