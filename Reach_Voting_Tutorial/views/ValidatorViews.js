import React from 'react';
import VoterViews from './VoterViews';

const exports = {...VoterViews};

const sleep = (milliseconds) => new Promise(resolve => setTimeout(resolve, milliseconds));

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <div className="header">
        <h4>Validator</h4>
        {content}
      </div>
    );
  }
}

exports.SetStake = class extends React.Component {
  render() {
    const {parent, defaultStake, standardUnit} = this.props;
    const stake = (this.state || {}).stake || defaultStake;
    return (
      <div>
        <input
          type='number'
          placeholder={defaultStake}
          onChange={(e) => this.setState({stake: e.currentTarget.value})}
        /> {standardUnit}
        <br />
        <button
          onClick={() => parent.setStake(stake)}
        >Set Stake</button>
      </div>
    );
  }
}

exports.Validator = class extends React.Component {
  render() {
    const {parent, stake, standardUnit} = this.props;
    return (
      <div>
        Set Stake: <strong>{stake}</strong> {standardUnit}
        <br />
        <button
          onClick={() => parent.validate()}
        >Make Contract</button>
      </div>
    );
  }
}

exports.Validating = class extends React.Component {
  render() {
    return (
      <div>Compiling...please wait</div>
    );
  }
}

exports.WaitingForMember = class extends React.Component {
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
    return (
      <div>
        Your assigned voter is joining....
        <br /> Send them this smart contract info through the chat
        <pre className='ContractInfo'>
          {ctcInfoStr}
        </pre>
        <button
          onClick={(e) => this.copyToClipborad(e.currentTarget)}
        >Copy to clipboard</button>
      </div>
    )
  }
}

export default exports;