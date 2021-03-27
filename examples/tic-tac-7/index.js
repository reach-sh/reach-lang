import React from "react";
import AppViews from "./views/AppViews";
import DeployerViews from "./views/DeployerViews";
import AttacherViews from "./views/AttacherViews";
import {renderDOM, renderView} from "./views/render";
import "./index.css";
import * as backend from "./build/index.main.mjs";
import * as reach from "@reach-sh/stdlib/ETH";

// currency default
const {standardUnit} = reach;

// If dev mode, use console.log(), else, don't
const dev = true;

// sleep function for delaying until move_made soud is played
const sleep = async (milliseconds) => await new Promise(resolve => setTimeout(resolve, milliseconds));

// convert keyboard numeric keypad input layout to reach back end
const handToInt = { // index to reach
    "box7": 0,
    "box8": 1,
    "box9": 2,
    "box4": 3,
    "box5": 4,
    "box6": 5,
    "box1": 6,
    "box2": 7,
    "box3": 8
};

const intToHand = { // index to gameplay
    1: "box7",
    2: "box8",
    3: "box9",
    4: "box4",
    5: "box5",
    6: "box6",
    7: "box1",
    8: "box2",
    9: "box3"
};

const X_image = "https://imgur.com/eBsDAXr.png";
const O_image = "https://imgur.com/h1xuIW1.png";

// set default variables, wager, account fund amount and standard unit
const defaults = {
    defaultFundAmt: "100",
    defaultWager: "5",
    standardUnit
};

// App class
class App extends React.Component {
    // Starting screen, aka Connecting accounts
    constructor(props) {
        super(props);
        this.state = {
            view: "ConnectAccount",
            ...defaults
        };
    }

    // When the player's account has connected, either ask them to fund their account or simply start the game
    async componentDidMount() {
        this.setState({
            view: "Start"
        });
    }

    async finalizeMount() {
        var acc = await reach.getDefaultAccount();
        if (dev) {
            console.log(acc)
        }
        const balAtomic = await reach.balanceOf(acc);
        const bal = reach.formatCurrency(balAtomic, 4);
        this.setState({
            acc,
            bal
        });
        try {
            const faucet = await reach.getFaucet();
            this.setState({
                view: "FundAccount",
                faucet
            });
        } catch (e) {
            this.setState({
                view: "DeployerOrAttacher"
            });
        }
    }

    // Fund the given account and then start the game
    async fundAccount(fundAmount) {
        await reach.transfer(
            this.state.faucet,
            this.state.acc,
            reach.parseCurrency(fundAmount)
        );
        this.setState({
            view: "DeployerOrAttacher"
        });
    }

    // Skip funding the account and continue to the game
    async skipFundAccount() {
        this.setState({
            view: "DeployerOrAttacher"
        });
    }

    // HAve the user select if they want to be an attacher or a deployer
    selectAttacher() {
        this.setState({
            view: "Wrapper",
            ContentView: Attacher
        });
    }
    selectDeployer() {
        this.setState({
            view: "Wrapper",
            ContentView: Deployer
        });
    }

    // return the final data (rendered)
    render() {
        return renderView(this, AppViews);
    }
}

// Create the class for a normal player
class Player extends React.Component {
    // create the random function for the reach file to access
    random() {
        return reach.hasRandom.random();
    }
    // get the move the user wants to play
    async getHand(x, xs, os) { // bool(x) whether its X's turn
        // Present the buttons to the user so that they can actually select their move
        // hand is a box ID number relative to numeric keypad
        let valid = 1; // occupied
        let hand = null;
        

        while (valid > 0) { // while occupied
            hand = await new Promise((resolveHandP) => {
                this.setState({
                    view: "GetHand",
                    resolveHandP,
                    xs, // 1d array of 1's 0's; 1 = occupied by X
                    os // 1d array of 1's 0's; 1 = occupied by O
                });

                for (let i = 1; i < 10; i++) {
                    if (dev) {
                        console.log(`${i} ${intToHand[i]} xs[i-1]=${parseInt(xs[i-1])} os[i-1]=${parseInt(os[i-1])}`);
                    }
                    if (parseInt(xs[i-1]) === 1) {
                        document.getElementById(intToHand[i]).src = X_image; 
                    }
                    if (parseInt(os[i-1]) === 1) {
                        document.getElementById(intToHand[i]).src = O_image;
                    }
            }
            });
            var blinger = document.getElementById("bling");
            blinger.play();

            valid = (
                parseInt(os[handToInt[hand]])
                + parseInt(xs[handToInt[hand]])
            ); // should return 1=occupied / 0=vacant
            if (dev) {
                console.log(`${valid}`);
            }

        };
        
        sleep(4000);

        if (dev) {
            console.log(`${os}`);
            console.log(`${xs}`);
        }
        
        // Display that a move as been accepted
        this.setState({
            view: "GetHandb",
            playable: true
        });

        document.getElementById(hand).src = {
            x: X_image, 
            o: O_image,
        }[x ? "x" : "o"];

        if (dev) {
            console.log(`${hand}`);
        }
        for (let i = 1; i < 10; i++) {
            if (dev) {
                console.log(`${i} ${intToHand[i]} xs[i-1]=${parseInt(xs[i-1])} os[i-1]=${parseInt(os[i-1])}`);
            }
            if (parseInt(xs[i-1]) === 1) {
                document.getElementById(intToHand[i]).src = X_image; 
            }
            if (parseInt(os[i-1]) === 1) {
                document.getElementById(intToHand[i]).src = O_image;
            }
        }

        // return the move
        return handToInt[hand];
    }

    // function to display who won
    endsWith(won, double, oppo, tie) {
        this.setState({
            view: "Finale",
            won: won,
            double: double,
            tie: tie
        });
    }

    // Inform the players that a timeout has occurred
    informTimeout() {
        this.setState({
            view: "Timeout"
        });
    }

    // returns hand to Reach
    setImg(box_id) {
        this.state.resolveHandP(box_id);
    }
}

// Create the class for things that only apply to a deployer
class Deployer extends Player {
    // Set the wager
    constructor(props) {
        super(props);
        this.state = {
            view: "SetWager"
        };
    }
    setWager(wager) {
        this.setState({
            view: "Deploy",
            wager
        });
    }

    // Deploy the contract
    async deploy() {
        const ctc = this.props.acc.deploy(backend);
        this.setState({
            view: "Deploying",
            ctc
        });
        this.wager = reach.parseCurrency(this.state.wager); // UInt
        backend.A(ctc, this);
        if (dev) {
            console.log(await reach.getDefaultAccount());
        }
        const ctcInfo = await ctc.getInfo();
        const ctcInfoStr = JSON.stringify(ctcInfo, null, 2);
        this.setState({
            view: "WaitingForAttacher",
            ctcInfoStr
        });
    }

    // render the data returned by the class
    render() {
        return renderView(this, DeployerViews);
    }
}

// Create the class for things that are specific to the Attacher
class Attacher extends Player {
    // Display the view for attaching to the deployer
    constructor(props) {
        super(props);
        this.state = {
            view: "Attach"
        };
    }
    // Actually attach
    attach(ctcInfoStr) {
        const ctc = this.props.acc.attach(backend, JSON.parse(ctcInfoStr));
        this.setState({
            view: "Attaching"
        });
        backend.B(ctc, this);
    }

    // accept the wager proposed by the deployer
    async acceptWager(wagerAtomic) {
        // Fun([UInt], Null)
        const wager = reach.formatCurrency(wagerAtomic, 4);
        return await new Promise((resolveAcceptedP) => {
            this.setState({
                view: "AcceptTerms",
                wager,
                resolveAcceptedP
            });
        });
    }
    // if the terms have been accepted, display that the player is waiting for his/her turn.
    termsAccepted() {
        this.state.resolveAcceptedP();
        this.setState({
            view: "WaitingForTurn"
        });
    }

    // render that data returned by this class
    render() {
        return renderView(this, AttacherViews);
    }
}

// render the entire script.
renderDOM( < App / > );
