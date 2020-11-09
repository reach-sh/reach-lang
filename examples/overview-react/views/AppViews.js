import React from 'react';

export class Wrapper extends React.Component {
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

export class ConnectAccount extends React.Component {
  render() {
    return (
      <div>
        Please wait while we connect to your account.
        If this takes more than a few seconds, there may be something wrong.
      </div>
    )
  }
}

export class FundAccount extends React.Component {
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

export class SelectRole extends React.Component {
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
