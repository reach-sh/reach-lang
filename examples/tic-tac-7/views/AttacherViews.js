import React from "react";
import PlayerViews from "./PlayerViews";

const exports = {...PlayerViews};

////////////////////////////////////////////////////////////////////////////////////////
// Wrapper for hte attacher to be displayed at the top of every page fo rht eattacher //
////////////////////////////////////////////////////////////////////////////////////////
/* eslint-disable */
exports.Wrapper = class extends React.Component {
    render() {
        const {content} = this.props;
        return (
            <div className="Attacher">
                <h2>YOU ARE:</h2>
                &nbsp;<img src="https://imgur.com/h1xuIW1.png" width="60px" height="60px"></img>
                {content}
            </div>
        );
    }
};

/////////////////////////////////////////////////////////////////
// Page for attaching to the contract deployed by the deployer //
/////////////////////////////////////////////////////////////////

exports.Attach = class extends React.Component {
    render() {
        const {parent} = this.props;
        const {ctcInfoStr} = this.state || {};
        return (
            <div>
                Please paste the contract info to attach to:
                <br />
                <br />
                <textarea
                    className="ContractInfoA"
                    onChange={(e) =>
                        this.setState({ctcInfoStr: e.currentTarget.value})
                    }
                    placeholder="{}"
                />
                <br />
                <br />
                <button
                    disabled={!ctcInfoStr}
                    onClick={() => parent.attach(ctcInfoStr)}
                >
                    Attach
                </button>
            </div>
        );
    }
};

//////////////////////////////////////////////////////////////////////
// Page for displaying that the player is attaching to the contract //
//////////////////////////////////////////////////////////////////////

exports.Attaching = class extends React.Component {
    render() {
        return <div>Attaching, please wait...</div>;
    }
};

//////////////////////////////////////////////
// Page for accepting the terms of the game //
//////////////////////////////////////////////

exports.AcceptTerms = class extends React.Component {
    render() {
        const {wager, standardUnit, parent} = this.props;
        const {disabled} = this.state || {};
        return (
            <div>
                The terms of the game are:
                <br /><br /> Wager: {wager} {standardUnit}
                <br /><br />
                <button
                    disabled={disabled}
                    onClick={() => {
                        this.setState({disabled: true});
                        parent.termsAccepted();
                    }} /*style={{background-size: 500px 50px, width:500, height=50}}*/
                >
                    Pay wager
                </button>
            </div>
        );
    }
};

//////////////////////////////////////////////////////////////
// Page to display that we are waiting for the other player //
//////////////////////////////////////////////////////////////

exports.WaitingForTurn = class extends React.Component {
    render() {
        return (
            <div>
                Waiting for the other player...
                <br />
                Get ready to play some serious Tic-Tac-Toe!
            </div>
        );
    }
};
/* eslint-disable */
export default exports;
