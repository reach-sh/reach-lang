import React from 'react';

export class RunBackend extends React.Component {
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

export class ApproveRequest extends React.Component {
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

export class DisplayInfo extends React.Component {
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

export class BobWrapper extends React.Component {
  render() {
    const {bob} = this.props;
    return (
      <div className='Bob'>
        {bob}
      </div>
    );
  }
}
