import React from 'react';

const sleep = (milliseconds) => new Promise(resolve => setTimeout(resolve, milliseconds));

export class Deploy extends React.Component {
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

export class EnterInfo extends React.Component {
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

export class EnterRequest extends React.Component {
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

export class RunBackend extends React.Component {
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

export class BackendRunning extends React.Component {
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

export class BackendRan extends React.Component {
  render() {
    return (
      <div>
        Thank you, Alice.
        The contract has run to completion.
      </div>
    );
  }
}

export class AliceWrapper extends React.Component {
  render() {
    const {alice} = this.props;
    return (
      <div className="Alice">
        {alice}
      </div>
    );
  }
}