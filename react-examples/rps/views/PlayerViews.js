import React from 'react';

const exports = {};

// Player views must be extended.
// It does not have its own Wrapper view.

exports.Done = class extends React.Component {
  render() {
    const {result} = this.props;
    return (
      <div>
        Thank you for playing. The result of this game was:
        <br />{result || 'Unknown'}
      </div>
    );
  }
}

exports.WaitingForTurn = class extends React.Component {
  render() {
    const {addr, partnerAddr} = this.props;
    return (
      <div>
        Waiting for the other player...
        <br />Think about which move you want to play.
        <br />
        <br /> You are: {addr}
        <br /> You are playing against: {partnerAddr}
      </div>
    );
  }
}

exports.GetHand = class extends React.Component {
  render() {
    const {parent, playable} = this.props;
    return (
      <div>
        {!playable ? 'Please wait...' : ''}
        <br />
        <button
          disabled={!playable}
          onClick={() => parent.playHand('ROCK')}
        >Rock</button>
        <button
          disabled={!playable}
          onClick={() => parent.playHand('PAPER')}
        >Paper</button>
        <button
          disabled={!playable}
          onClick={() => parent.playHand('SCISSORS')}
        >Scissors</button>
      </div>
    );
  }
}

exports.WaitingForResults = class extends React.Component {
  render() {
    const {addr, partnerAddr} = this.props;
    return (
      <div>
        Waiting for results...
        <br /> You are: {addr}
        <br /> You are playing against: {partnerAddr}
      </div>
    );
  }
}

export default exports;
