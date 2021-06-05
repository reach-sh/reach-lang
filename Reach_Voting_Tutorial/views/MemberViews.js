import React from 'react';
import VoterViews from './VoterViews';

const exports = {...VoterViews};

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <div className="Member">
        <h2>Member</h2>
        {content}
      </div>
    );
  }
}

exports.Secure = class extends React.Component {
  render() {
    const {parent} = this.props;
    const {ctcInfoStr} = this.state || {};
    return (
      <div>
        Please enter the transaction info that was sent to you by your validator:
        <br />
        <textarea spellcheck="false"
          className='ContractInfo'
          onChange={(e) => this.setState({ctcInfoStr: e.currentTarget.value})}
          placeholder='{}'
        />
        <br />
        <button
          disabled={!ctcInfoStr}
          onClick={() => parent.attach(ctcInfoStr)}
        >Validate</button>
      </div>
    );
  }
}

exports.Securing = class extends React.Component {
  render() {
    return (
      <div>
        Validating....please wait....
      </div>
    );
  }
}

exports.AcceptStake = class extends React.Component {
  render() {
    const {stake, standardUnit, parent} = this.props;
    const {disabled} = this.state || {};
    return (
      <div>
        The stake and ticket price as dictated by the validator are....
        <br /> stake: {stake} {standardUnit}
        <br />
        <button
          disabled={disabled}
          onClick={() => {
            this.setState({disabled: true});
            parent.termsAccepted();
          }}
        >Pay Ticket Price and Stake!</button>
      </div>
    );
  }
}

exports.WaitingForTurn = class extends React.Component {
  render() {
    return (
      <div>
        Waiting.....
        <br />Open the chat in the new tab to discuss more!
      </div>
    );
  }
}

export default exports;