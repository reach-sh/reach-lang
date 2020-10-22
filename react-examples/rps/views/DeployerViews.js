import React from 'react';
import PlayerViews from './PlayerViews';

const exports = {...PlayerViews};

const sleep = (milliseconds) => new Promise(resolve => setTimeout(resolve, milliseconds));

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <div className="Deployer">
        <h2>Deployer (Alice)</h2>
        {content}
      </div>
    );
  }
}

exports.SetWager = class extends React.Component {
  render() {
    const {parent, defaultWager, standardUnit} = this.props;
    this.state = this.state || {};
    const wager = this.state.wager || defaultWager;
    return (
      <div>
        <input
          type='number'
          placeholder={defaultWager}
          onChange={(e) => this.setState({wager: e.currentTarget.value})}
        /> {standardUnit}
        <br />
        <button
          onClick={() => parent.setWager(wager)}
        >Set wager</button>
      </div>
    );
  }
}

exports.SetEscrow = class extends React.Component {
  render() {
    const {parent, defaultEscrow, standardUnit} = this.props;
    this.state = this.state || {};
    const escrow = this.state.escrow || defaultEscrow;
    return (
      <div>
        <input
          type='number'
          placeholder={defaultEscrow}
          onChange={(e) => this.setState({escrow: e.currentTarget.value})}
        /> {standardUnit}
        <br />
        <button
          onClick={() => parent.setEscrow(escrow)}
        >Set escrow</button>
      </div>
    )
  }
}

exports.Deploy = class extends React.Component {
  render() {
    const {parent, wager, escrow, total, standardUnit} = this.props;
    return (
      <div>
        Wager: <strong>{wager}</strong> {standardUnit}
        <br />
        Escrow: <strong>{escrow}</strong> {standardUnit}
        <br />
        Total cost to deploy (excluding gas): <strong>{total}</strong> {standardUnit}
        <br />
        <button
          onClick={() => parent.deploy()}
        >Deploy</button>
      </div>
    );
  }
}

exports.Deploying = class extends React.Component {
  render() {
    return (
      <div>Deploying... please wait.</div>
    );
  }
}

exports.WaitingForAttacher = class extends React.Component {
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
        Waiting for Attacher to join...
        <br /> Please give them this contract info:
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
