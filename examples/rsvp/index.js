import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import * as backend from './build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
import MyAlgoConnect from '@randlabs/myalgo-connect';
const stdlib = loadStdlib(process.env);
const providerEnv = undefined; // 'TestNet'
stdlib.setWalletFallback(reach.walletFallback({
  providerEnv, MyAlgoConnect }));
const {standardUnit} = stdlib;
const defaultPrice = '20';
const defaultDeadline = '50';

function renderDOM() {
  ReactDOM.render(
    <React.StrictMode><App /></React.StrictMode>,
    document.getElementById('root')
  );
}

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'Connect'}
  }
  async componentDidMount() {
    const acc = await stdlib.getDefaultAccount();
    try {
      const faucet = await stdlib.getFaucet();
      stdlib.transfer(faucet, acc, stdlib.parseCurrency(100));
    } catch (e) {
      console.error(e);
    }
    this.setState({mode: 'Select', acc });
  }
  selectRole(role) { this.setState({mode: 'RunRole', role}); }
  doCreate()  { this.selectRole(<Create  acc={this.state.acc} />); }
  doRSVP()    { this.selectRole(<RSVP    acc={this.state.acc} />); }
  doCheckin() { this.selectRole(<Checkin acc={this.state.acc} />); }
  doClose()   { this.selectRole(<Close   acc={this.state.acc} />); }
  render() {
    const {mode, addr, bal, role} = this.state;
    const parent = this;
    let app = null;
    if (mode === 'Connect') {
      app = (
        <div>
          Please wait while we connect to your account.
          If this takes more than a few seconds, there may be something wrong.
        </div>
      )
    } else if (mode === 'Select') {
      app = (
        <div>
          <p>
            <button
              onClick={() => parent.doCreate()}
            >Create</button>
            <br />
            Create an event
          </p>
          <p>
            <button
              onClick={() => parent.doRSVP()}
            >RSVP</button>
            <br />
            RSVP for an event
          </p>
          <p>
            <button
              onClick={() => parent.doCheckin()}
            >Checkin</button>
            <br />
            Check-in at an event
          </p>
          <p>
            <button
              onClick={() => parent.doClose()}
            >Close</button>
            <br />
            Close an event
          </p>
        </div>
      );
    } else { // 'RunRole'
      app = role;
    }
    return (
      <div className="App">
        <header className="App-header" id="root">
          {app}
        </header>
      </div>
    );
  }
}

class Create extends React.Component {
  constructor(props) {
    super(props);
    this.setState({mode: 'EnterInfo'});
  }
  async enterInfo(priceStandard, deadline) {
    const ctc = this.props.acc.contract(backend);
    this.setState({mode: 'Wait', priceStandard, deadline, ctc});
    console.log({priceStandard, deadline});
    try {
      await ctc.p.Admin({
        price: stdlib.parseCurrency(priceStandard),
        deadline: stdlib.bigNumberify(deadline),
        ready: () => {
          throw 42;
        }
      });
    } catch (e) {
      if ( e !== 42 ) {
        throw e;
      }
    }
    const ctcInfoStr = JSON.stringify(await ctc.getInfo(), null, 2);
    this.setState({mode: 'Done', ctcInfoStr});
  }
  render() {
    let me = null;
    const parent = this;
    const mode = this.state?.mode || 'EnterInfo';
    if (mode === 'EnterInfo') {
      const priceStandard = this.state?.priceStandard || defaultPrice;
      const deadline = this.state?.deadline || defaultDeadline;
      me = (
        <div>
          What is the RSVP fee?
          <br />
          <textarea
            onChange={(e) => this.setState({
              priceStandard: e.currentTarget.value})}
            placeholder={defaultPrice}
          />
          <br />
          What is the deadline?
          <br />
          <textarea
            onChange={(e) => this.setState({
              deadline: e.currentTarget.value})}
            placeholder={defaultDeadline}
          />
          <br />
          <button onClick={() => parent.enterInfo(
            priceStandard,
            deadline,
          )}
          >Launch</button>
        </div>
      );
    } else if (mode === 'Wait') {
      me = (
        <div>
          Please wait while your event is initialized.
        </div>
      );
    } else { // 'Done'
      const ctcInfoStr = this.state?.ctcInfoStr || '';
      me = (
        <div>
          Your event is ready for users to RSVP to!
          <br />

          Please share the following contract info with them:

          <pre className='ContractInfo'>
            {ctcInfoStr}
          </pre>
        </div>
      );
    }
    return (
      <div className="Create">
        {me}
      </div>
    );
  }
}

class RSVP extends React.Component {
  constructor(props) {
    super(props);
    this.setState({mode: 'EnterInfo'});
  }
  async doRSVP(ctcInfoStr) {
    const ctcInfo = JSON.parse(ctcInfoStr);
    const ctc = this.props.acc.contract(backend, ctcInfo);
    this.setState({mode: 'Wait', ctc});
    await ctc.apis.Attendee.iWillGo();
    this.setState({mode: 'Done'});
  }
  render() {
    let me = null;
    const parent = this;
    const mode = this.state?.mode || 'EnterInfo';
    if (mode === 'EnterInfo') {
      const ctcInfoStr = this.state?.ctcInfoStr || '';
      me = (
        <div>
          What is the event info?
          <br />
          <textarea
            className='ContractInfo'
            spellCheck='false'
            onChange={(e) => this.setState({ctcInfoStr: e.currentTarget.value})}
            placeholder='{}'
          />
          <br />
          <button
            disabled={!ctcInfoStr}
            onClick={() => parent.doRSVP(ctcInfoStr)}
          >RSVP</button>
        </div>
      );
    } else if (mode === 'Wait') {
      me = (
        <div>
          Please wait while your RSVP is confirmed.
        </div>
      );
    } else { // 'Done'
      const {acc} = this.props;
      me = (
        <div>
          You have RSVP'd.
          <br />

          Your address is:

          <pre className='ContractInfo'>
            {stdlib.formatAddress(acc)}
          </pre>
        </div>
      );
    }
    return (
      <div className="RSVP">
        {me}
      </div>
    );
  }
}

class Checkin extends React.Component {
  constructor(props) {
    super(props);
    this.setState({mode: 'EnterInfo'});
  }
  async doCheckin(ctcInfoStr, who) {
    const ctcInfo = JSON.parse(ctcInfoStr);
    const ctc = this.props.acc.contract(backend, ctcInfo);
    this.setState({mode: 'Wait', ctc, who});
    await ctc.apis.Checkin.theyCame(who);
    this.setState({mode: 'Done'});
  }
  render() {
    let me = null;
    const parent = this;
    const mode = this.state?.mode || 'EnterInfo'
    if (mode === 'EnterInfo') {
      const ctcInfoStr = this.state?.ctcInfoStr || '';
      const who = this.state?.who || '';
      me = (
        <div>
          What is the event info?
          <br />
          <textarea
            className='ContractInfo'
            spellCheck='false'
            onChange={(e) => this.setState({ctcInfoStr: e.currentTarget.value})}
            placeholder='{}'
          />
          <br />
          Who is checking in?
          <br />
          <textarea
            onChange={(e) => this.setState({
              who: e.currentTarget.value})}
          />
          <br />
          <button
            disabled={!ctcInfoStr}
            onClick={() => parent.doCheckin(ctcInfoStr, who)}
          >RSVP</button>
        </div>
      );
    } else if (mode === 'Wait') {
      me = (
        <div>
          Please wait while your checkin is confirmed.
        </div>
      );
    } else { // 'Done'
      const who = this.state?.who || '';
      me = (
        <div>
          Done! You have checked in {who}.
          <br />
        </div>
      );
    }
    return (
      <div className="Checkin">
        {me}
      </div>
    );
  }
}

class Close extends React.Component {
  constructor(props) {
    super(props);
    this.setState({mode: 'EnterInfo'});
  }
  async doClose(ctcInfoStr) {
    const ctcInfo = JSON.parse(ctcInfoStr);
    const ctc = this.props.acc.contract(backend, ctcInfo);
    this.setState({mode: 'Wait', ctc});
    await ctc.apis.Checkin.timesUp();
    this.setState({mode: 'Done'});
  }
  render() {
    let me = null;
    const parent = this;
    const mode = this.state?.mode || 'EnterInfo';
    if (mode === 'EnterInfo') {
      const ctcInfoStr = this.state?.ctcInfoStr || '';
      me = (
        <div>
          What is the event info?
          <br />
          <textarea
            className='ContractInfo'
            spellCheck='false'
            onChange={(e) => this.setState({ctcInfoStr: e.currentTarget.value})}
            placeholder='{}'
          />
          <br />
          <button
            disabled={!ctcInfoStr}
            onClick={() => parent.doClose(ctcInfoStr)}
          >Close</button>
        </div>
      );
    } else if (mode === 'Wait') {
      me = (
        <div>
          Please wait while your close is confirmed.
        </div>
      );
    } else { // 'Done'
      me = (
        <div>
          You have closed the event.
        </div>
      );
    }
    return (
      <div className="Close">
        {me}
      </div>
    );
  }
}

renderDOM();
