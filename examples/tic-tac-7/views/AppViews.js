import React from "react";

const exports = {};

// FIXME:  Add start screen
// exports.Start = class extends React.Component

/////////////////////////////////////////////////////////////////////////
// The content defined here will be displayed at the top of every page //
/////////////////////////////////////////////////////////////////////////

exports.Wrapper = class extends React.Component {
    render() {
        const {content} = this.props;
        return (
            <div className="App">
                <br />
                <img
                    src="https://i.imgur.com/Z5BHZ4E.png"
                    alt="title"
                    style={{width: 700, height: 100}}
                ></img>
                <header className="App-header" id="root">
                    {content}
                </header>
            </div>
        );
    }
};

//
//
//

exports.Start = class extends React.Component {
    render() {
        const {parent} = this.props;
        return (
            <div>
            <br />
            <br />
            <center>
            <div id="bullutin">
                <pre style={{fontSize: 17, fontFamily: "monospace"}}>
                    <p style={{fontSize:28}}>RULES OF THE GAME</p>
                    <hr />
                    <br />  1) A maximum total game wager is agreed to at the start of the game.                                               .
                    <br />  2) The costs for each move are:                                                                                    .
                    <br />      - 4/16s of your wager for the middle                                                                           .
                    <br />      - 3/16s of your wager for corners                                                                              .
                    <br />      - 2/16s of your wager for sides                                                                                .
                    <br />  3) If your opponent gets a double win, you lose your full wager.                                                   .
                    <br />  4) Each player is also personally responsible for transaction fees.                                                .
                    <br />  5) X always pays the initial contract transaction fee.                                                         .
                    <br />  6) If you decide to remove yourself from the game before it ends, you lose everything you have put in the pot.     .
                    <br />  7) Upon a tie game, you lose everything you have put in the pot and gain your opponent's expenditure.              .
                    <br />  8) The person to play the first move is chosen randomly.                                                           .
                </pre>
                </div>
            </center>
                <br />
                <br />
                <button onClick={() => parent.finalizeMount()}>I Agree</button>

            </div>
        );
    }
};

///////////////////////////////////////////////////
// Create the page for connecting to the account //
///////////////////////////////////////////////////

exports.ConnectAccount = class extends React.Component {
    render() {
        return (
            <div>
                Please wait while we connect to your account. If this takes more
                than a few seconds, there may be something wrong.
            </div>
        );
    }
};

/////////////////////////////////////////////////
// Fund a devnet account if the user so wishes //
/////////////////////////////////////////////////

exports.FundAccount = class extends React.Component {
    render() {
        const {bal, standardUnit, defaultFundAmt, parent} = this.props;
        const amt = (this.state || {}).amt || defaultFundAmt;
        return (
            <div>
                <h2>Fund account</h2>
                <br />
                Balance: {bal} {standardUnit}
                <hr />
                Would you like to fund your account with additional{" "}
                {standardUnit}?
                <br />
                (This only works on certain devnets)
                <br /><br />
                <input
                    type="number"
                    placeholder={defaultFundAmt}
                    onChange={(e) =>
                        this.setState({amt: e.currentTarget.value})
                    }
                />
                &nbsp;&nbsp;&nbsp;&nbsp;              
                <button onClick={() => parent.fundAccount(amt)}>
                    Fund Account
                </button>
                &nbsp;&nbsp;&nbsp;&nbsp;
                <button onClick={() => parent.skipFundAccount()}>Skip</button>
            </div>
        );
    }
};

////////////////////////////////////////////////////////
// Ask if a player wants to be a deployer or attacher //
////////////////////////////////////////////////////////

exports.DeployerOrAttacher = class extends React.Component {
    render() {
        const {parent} = this.props;
        return (
            <div>
                Please select a role:
                <br />
                <br />
                <br />
                <p>
                    <button onClick={() => parent.selectDeployer()}>
                        X
                    </button>
                    <br /> Set wager and deploy a contract as X
                </p>
                <br/>
                <br />
                <p>
                    <button onClick={() => parent.selectAttacher()}>
                        O
                    </button>
                    <br /> Agree to X's wager and join the contract as O
                </p>
            </div>
        );
    }
};

export default exports;
