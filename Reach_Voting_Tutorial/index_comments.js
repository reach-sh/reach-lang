// 17 U.S.C. §§ 101-800.
// Copyright Archie Chaudhury and Brian Haney 2021
// MIT License
// Permission is hereby granted, free of charge, 
// to any person obtaining a copy of this software and associated documentation files (the "Software"), 
// to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, 
// and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

// The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, 
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. 
// IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
// ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
////////////////////////////////////////////////////////////////////////////

// Voting is an ancient tradition in human history. 
// The Ancient Greeks voted by raising their hands in early democracies.
// Today, our votes are scattered across global information networks.
// Still, voting demends on the system's integrity.
// This tutorial introduces a new kind of voting.
////////////////////////////////////////////////////////////////////////////

// Lines 1-9 are the standard imports.
// The imports include the standard library, views, and stylesheet.
// Additionally, the backend is imported. 
// Line 1 imports react.
// Lines 2 - 5 imports the views file.
// Line 6 imports the index file.
// Line 7 is the backend import.
// Line 8 is the standard library import.
////////////////////////////////////////////////////////////////////////////
import React from 'react';
import AppViews from './views/AppViews';
import ValidatorViews from './views/ValidatorViews';
import MemberViews from './views/MemberViews';
import {renderDOM, renderView} from './views/render';
import './index.css';
import * as backend from './build/index.main.mjs';
import * as reach from '@reach-sh/stdlib/ALGO';
////////////////////////////////////////////////////////////////////////////

// Lines 10-13 define various variables.
// Line 10 declares the vote_array and assigns numbers to each one of them.
// Line 11 declares the outcome array and ensures that the addition leads to a result.
////////////////////////////////////////////////////////////////////////////
const vote_array = {'No': 0, 'Yes': 1};
const outcome_array = ['Alice does not get 5 Algo', 'Consensus Not Reached', 'Alice gets 5 Algo!'];
const {standardUnit} = reach;
const defaults = {defaultFundAmt: '30', defaultStake: '10', standardUnit};
////////////////////////////////////////////////////////////////////////////

// Lines 15-19 Create application class.
// Line 15 defines a class for react.
////////////////////////////////////////////////////////////////////////////
class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {view: 'ConnectAccount', ...defaults};
  }

  // Lines 20-31 define the initial variables for the application.
  // Line 20 defines the componentDidMount() function.
  ////////////////////////////////////////////////////////////////////////////
  async componentDidMount() {
    const acc = await reach.getDefaultAccount();
    const balAtomic = await reach.balanceOf(acc);
    const bal = reach.formatCurrency(balAtomic, 4);
    this.setState({acc, bal});
    try {
      const faucet = await reach.getFaucet();
      this.setState({view: 'FundAccount', faucet});
    } catch (e) {
      this.setState({view: 'ValidatorOrMember'});
    }
  }
  
  // Lines 32-35 defines a fund account function.
  // Line 34 sets the state for the validator or member.
  // This function allows the user to fund their account before the voting process.
  ////////////////////////////////////////////////////////////////////////////
  async fundAccount(fundAmount) {
    await reach.transfer(this.state.faucet, this.state.acc, reach.parseCurrency(fundAmount));
    this.setState({view: 'ValidatorOrMember'});
  }

  // Lines 36 - 40 define additional functions, allowing the member to skip over the funding process.
  ////////////////////////////////////////////////////////////////////////////
  async skipFundAccount() { this.setState({view: 'ValidatorOrMember'}); }
  selectMember() { this.setState({view: 'Wrapper', ContentView: Member}); }
  selectValidator() { this.setState({view: 'Wrapper', ContentView:Validator}); }
  render() { return renderView(this, AppViews); }
}

// Lines 42 -54
// Sets the definiton for a vote and allows the vote to be read by the backend for caluclation.
// Allows the user to see the result as well through a view
// Line 44 is the getVote function.
////////////////////////////////////////////////////////////////////////////
class Voter extends React.Component {
  random() { return reach.hasRandom.random(); }
  async getVote() { 
    const vote = await new Promise(resolveVoteP => {
      this.setState({view: 'GetVote', workable: true, resolveVoteP});
    });
    this.setState({view: 'WaitingForResults', vote});
    return vote_array[vote];
  }
  seeResult(i) { this.setState({view: 'Done', result: outcome_array[i]}); }
  informTimeout() { this.setState({view: 'Timeout'}); }
  workVote(vote) { this.state.resolveVoteP(vote); }
}

// Lines 56 - 71 define a validator class and set function's variables.
// Line 56 defines the validator class.
// The validator class can set a stake price for their assigned voter.
// Line 61 defines the set stake function.
// Line 62 defiens a validate function.
// Line 70 is a render funciton.
////////////////////////////////////////////////////////////////////////////
class Validator extends Voter {
  constructor(props) {
    super(props);
    this.state = {view: 'SetStake'};
  }
  setStake(stake) { this.setState({view: 'Validator', stake}); }
  async validate() {
    const ctc = this.props.acc.deploy(backend);
    this.setState({view: 'Validating', ctc});
    this.stake = reach.parseCurrency(this.state.stake); 
    backend.Alice(ctc, this);
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({view: 'WaitingForMember', ctcInfoStr});
  }
  render() { return renderView(this,ValidatorViews); }
}

// Lines 73 - 88 define  a member class.
// Line 73 defines the member class.
// Line 83 defines the accept stake function.
////////////////////////////////////////////////////////////////////////////
class Member extends Voter {
  constructor(props) {
    super(props);
    this.state = {view: 'Secure'};
  }

  // Lines 78 - 89 defines a new contract and stake.
  ////////////////////////////////////////////////////////////////////////////
  attach(ctcInfoStr) {
    const ctc = this.props.acc.attach(backend, JSON.parse(ctcInfoStr));
    this.setState({view: 'Securing'});
    backend.Bob(ctc, this);
  }
  async acceptStake(stakeAtomic) { 
    const stake = reach.formatCurrency(stakeAtomic, 4);
    return await new Promise(resolveAcceptedP => {
      this.setState({view: 'AcceptStake', stake, resolveAcceptedP});
    });
  }
  
  // Lines 89 - 94 define the Member class, who is assigned a validator and can accept the stake.
  // Line 93 is a rendering funciton.
  ////////////////////////////////////////////////////////////////////////////
  termsAccepted() {
    this.state.resolveAcceptedP();
    this.setState({view: 'WaitingForTurn'});
  }
  render() { return renderView(this, MemberViews); }
}

renderDOM(<App />);