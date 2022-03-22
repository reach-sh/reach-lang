import React from 'react';
import ReactDOM from 'react-dom';
import './index.css';
import * as backend from './build/index.main.mjs';
import {loadStdlib} from '@reach-sh/stdlib';
const stdlib = loadStdlib(process.env);
const {standardUnit} = stdlib;
const defaultFundAmtStandard = '10';
const defaultInfo = 'the cake is a lie';
const defaultRequestStandard = '0.5';

function renderDOM() {
  ReactDOM.render(
    <React.StrictMode><App /></React.StrictMode>,
    document.getElementById('root')
  );
}

class Wrapper extends React.Component {
  render() {
    const {app} = this.props;
    return (
      <div className="App">
        <header className="App-header" id="root">
          {app}
        </header>
      </div>
    );
  }
}

class ConnectAccount extends React.Component {
  render() {
    return (
      <div>
        Please wait while we connect to your account.
        If this takes more than a few seconds, there may be something wrong.
      </div>
    )
  }
}

class FundAccount extends React.Component {
  constructor(props) {
    super(props);
    this.state = {amt: props.defaultFundAmtStandard};
  }

  render() {
    const {addr, bal, standardUnit, defaultFundAmtStandard, parent} = this.props;
    return (
      <div>
        <h1>Fund account</h1>
        <br />
        Address: {addr}
        <br />
        Balance: {bal} {standardUnit}
        <hr />
        Would you like to fund your account with additional {standardUnit}?
        <br />
        (This only works on certain devnets)
        <br />
        <input
          type='number'
          placeholder={defaultFundAmtStandard}
          onChange={(e) => this.setState({amt: e.currentTarget.value})}
        />
        <button onClick={() => parent.fundAccount(this.state.amt)}>Fund Account</button>
        <button onClick={() => parent.skipFundAccount()}>Skip</button>
      </div>
    );
  }
}

class SelectRole extends React.Component {
  render() {
    const {parent} = this.props;
    return (
      <div>
        Please select a role:
        <br />
        <p>
          <button
            onClick={() => parent.selectAlice()}
          >Alice</button>
          <br /> Requests payment from Bob in order to reveal a secret.
        </p>
        <p>
          <button
            onClick={() => parent.selectBob()}
          >Bob</button>
          <br /> Pays Alice in order for her to reveal a secret.
        </p>
      </div>
    );
  }
}

class App extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'ConnectAccount'}
  }
  async componentDidMount() { // from mode: ConnectAccount
    const acc = await stdlib.getDefaultAccount();
    const addr = stdlib.formatAddress(await acc.getAddress());
    const balAtomic = await stdlib.balanceOf(acc);
    const bal = stdlib.formatCurrency(balAtomic, 4);
    try {
      const faucet = await stdlib.getFaucet();
      this.setState({mode: 'FundAccount', acc, addr, bal, faucet});
    } catch (e) {
      this.setState({mode: 'SelectRole', acc, addr, bal});
    }
  }
  fundAccount(fundAmountStandard) { // from mode: FundAccount
    const {faucet, acc} = this.state;
    const amountAtomic = stdlib.parseCurrency(fundAmountStandard || defaultFundAmtStandard);
    stdlib.transfer(faucet, acc, amountAtomic);
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

class Deploy extends React.Component {
  render() {
    const {parent} = this.props;
    return (
      <div>
        As Alice, it is your job to deploy the contract.
        <br />
        <button
          onClick={() => parent.deploy()}
        >Deploy</button>
      </div>
    );
  }
}

class EnterInfo extends React.Component {
  render() {
    const {parent, defaultInfo} = this.props;
    const {info} = this.state || {};
    return (
      <div>
        Alice, what is your secret info?
        <br />
        <textarea
          onChange={(e) => this.setState({info: e.currentTarget.value})}
          placeholder={defaultInfo}
        />
        <br />
        <button onClick={() => parent.enterInfo(info || defaultInfo)}
        >Submit secret info</button>
      </div>
    );
  }
}

class EnterRequest extends React.Component {
  render() {
    const {parent, standardUnit, defaultRequestStandard} = this.props;
    const {req} = this.state || {};
    return (
      <div>
        Alice, how much {standardUnit} should Bob pay you
        to reveal this info?
        <br />
        <input
          type='number'
          onChange={(e) => this.setState({req: e.currentTarget.value})}
          placeholder={defaultRequestStandard}
        />
        <br />
        <button onClick={() => parent.enterRequest(req || defaultRequestStandard)}
        >Submit request</button>
      </div>
    );
  }
}

class AliceRunBackend extends React.Component {
  render() {
    const {parent, info, requestStandard, standardUnit} = this.props;
    return (
      <div>
        <p>
          You request <strong>{requestStandard}</strong> {standardUnit + ' '}
          to reveal secret info: <strong>{info}</strong>
        </p>
        <p>
          Ready to connect to the contract?
        </p>
        <p>
          You will be prompted to pay for two transactions.
          The first transaction will publish your requested amount,
          and the second will publish your secret while simultaneously
          retrieving the requested amount from the contract.
        </p>
        <button
          onClick={() => parent.runBackend()}
        >Connect</button>
      </div>
    );
  }
}

class BackendRunning extends React.Component {
  async copyToClipborad(button) {
    const {ctcInfoStr} = this.props;
    navigator.clipboard.writeText(ctcInfoStr);
    const origInnerHTML = button.innerHTML;
    button.innerHTML = 'Copied!';
    button.disabled = true;
    await sleep(1000);
    button.innerHTML = origInnerHTML;
    button.disabled = false;
  }

  render() {
    const {ctcInfoStr} = this.props;
    if (ctcInfoStr === undefined) {
      return (
        <div>
          Waiting for the contract to deploy...
          If this takes more than 1 min, something may be wrong.
        </div>
      )
    } else {
      return (
        <div>
          <h2>Contract Info</h2>
          The contract is running!
          Please give Bob the following contract info.

          <pre className='ContractInfo'>
            {ctcInfoStr}
          </pre>
          <br />
          <button
            onClick={async (e) => this.copyToClipborad(e.currentTarget)}
          >Copy to clipboard</button>
          <br />

          You will be automatically prompted to approve the next transaction
          once Bob has paid the requested amount into the contract.
        </div>
      );
    }
  }
}

class BackendRan extends React.Component {
  render() {
    return (
      <div>
        Thank you, Alice.
        The contract has run to completion.
      </div>
    );
  }
}

class AliceWrapper extends React.Component {
  render() {
    const {alice} = this.props;
    return (
      <div className="Alice">
        {alice}
      </div>
    );
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
    const request = stdlib.parseCurrency(requestStandard);
    await backend.Alice(ctc, {request, info});
    this.setState({mode: 'BackendRan'});
  }
  render() {
    let alice = null;
    const parent = this;
    const {mode, ctcInfoStr, requestStandard, info} = this.state;
    if (mode === 'Deploy') {
      alice = <Deploy {...{parent}} />;
    } else if (mode === 'EnterInfo') {
      alice = <EnterInfo {...{parent, defaultInfo}} />;
    } else if (mode === 'EnterRequest') {
      alice = <EnterRequest {...{parent, standardUnit, defaultRequestStandard}} />;
    } else if (mode === 'RunBackend') {
      alice = <AliceRunBackend {...{parent, info, requestStandard, standardUnit}} />;
    } else if (mode === 'BackendRunning') {
      alice = <BackendRunning {...{ctcInfoStr}} />;
    } else { // 'BackendRan'
      alice = <BackendRan />;
    }
    return <AliceWrapper {...{alice}} />
  }
}

class BobRunBackend extends React.Component {
  render() {
    const {parent} = this.props;
    const {ctcInfoStr} = this.state || {};
    return (
      <div>
        Alice will deploy the contract.
        <br />
        Ask Alice for her contract info and paste it here:
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
          onClick={() => parent.runBackend(ctcInfoStr)}
        >Connect</button>
      </div>
    );
  }
}

class ApproveRequest extends React.Component {
  render() {
    const {requestStandard} = this.props;
    if (!requestStandard) {
      return (
        <p>
          Once Alice has submitted her requested amount,
          you will be prompted to pay it.
        </p>
      );
    } else {
      return (
        <p>
          You have received a prompt to pay Alice's requested amount.
        </p>
      );
    }
  }
}

class DisplayInfo extends React.Component {
  render() {
    const {info} = this.props;
    if (!info) {
      return (
        <p>
          Waiting for Alice to reveal her secret info...
        </p>
      );
    } else {
      return (
        <div>
          <p>
            Alice's secret info is: <strong>{info}</strong>
          </p>
          <p>
            Thank you, Bob. The contract has run to completion.
          </p>
        </div>
      );
    }
  }
}

class BobWrapper extends React.Component {
  render() {
    const {bob} = this.props;
    return (
      <div className='Bob'>
        {bob}
      </div>
    );
  }
}

class Bob extends React.Component {
  constructor(props) {
    super(props);
    this.state = {mode: 'RunBackend'};
  }
  async runBackend(ctcInfoStr) { // from mode: RunBackend
    const ctcInfo = JSON.parse(ctcInfoStr);
    const ctc = this.props.acc.contract(backend, ctcInfo);
    this.setState({mode: 'ApproveRequest'});
    const interact = {
      want: (request) => this.setState({mode: 'DisplayInfo', requestStandard: stdlib.formatCurrency(request, 4)}),
      got: (info) => this.setState({info}),
    };
    await backend.Bob(ctc, interact);
  }
  render() {
    let bob = null;
    const parent = this;
    const {mode, requestStandard, info} = this.state;
    if (mode === 'RunBackend') {
      bob = <BobRunBackend {...{parent}} />
    } else if (mode === 'ApproveRequest') {
      bob = <ApproveRequest {...{requestStandard}} />;
    } else { // 'DisplayInfo'
      bob = <DisplayInfo {...{info}} />
    }
    return <BobWrapper {...{bob}} />;
  }
}

renderDOM();
