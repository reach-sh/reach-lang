import React from 'react';

const exports = {};

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <div className="App">
        <header className="App-header" id="root">
          <h5>Tim's Thieves Decentralized Voting Algorithm</h5>
          {content}
        </header>
          <a className = "link" href = "https://daochat.loca.lt">Go to your organization's private chat!</a>
      </div>
    );
  }
}

exports.ConnectAccount = class extends React.Component {
  render() {
    return (
      <div>
        Connecting to your account....
        <div class="lds-ring"><div></div><div></div><div></div><div></div></div>
      </div>
    )
  }
}

exports.FundAccount = class extends React.Component {
  render() {
    const {bal, standardUnit, defaultFundAmt, parent} = this.props;
    const amt = (this.state || {}).amt || defaultFundAmt;
    return (
      <div>
        <h4>Add some Funds to your account</h4>
        <br />
        Current Balance For Staking: {bal} {standardUnit}
        <hr />
      Fund account with additional {standardUnit}?
        <br />
      Does not work with all wallets. Your organization(Tim's Thieves) must have enough ETH to fund for staking perspectives.
        <br />
        <input
          type='number'
          placeholder={defaultFundAmt}
          onChange={(e) => this.setState({amt: e.currentTarget.value})}
        />
        <button onClick={() => parent.fundAccount(amt)}>Add ETH(will cost gas)</button>
        <button onClick={() => parent.skipFundAccount()}>Go straight to voting</button>
      </div>
    );
  }
}

exports.ValidatorOrMember = class extends React.Component {
  render() {
    const {parent} = this.props;
    return (
      <div>
        Please select your organizational role(will be communicated through your organization)
        <br />
        <p>
          <button
            onClick={() => parent.selectMember()}
          >Member</button>
          <br />Regular Member within the organization. Has the ability to vote on propositions.
        </p>
        <p>
          <button
            onClick={() => parent.selectValidator()}
          >Validator</button>
          <br />Will ensure voter exists within the organization and then also paticipate in the voting process.
        </p>
      </div>
    );
  }
}

export default exports;