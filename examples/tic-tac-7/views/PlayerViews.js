import React from "react";

const exports = {};

////////////////////////////////////////////
// Player views must be extended.         //
// It does not have its own Wrapper view. //
////////////////////////////////////////////

/////////////////////////////////////
// Page for winning/losing message //
/////////////////////////////////////

exports.Finale = class extends React.Component {
    render() {
        const {won, double, tie} = this.props;
            let outcome = "";
            if (double) {
                outcome = "YOU WON TWO WAYS: YOU GAIN THE FULL GAME WAGER";
            } else if (tie) {
                outcome = "TIE: YOU GAIN YOUR OPPONENT'S EXPENDITURE AND LOSE YOUR OWN";
            } else if (won) {
                outcome = "YOU WON: YOU GAIN YOUR OPPONENT'S EXPENDITURE";
            } else {
                outcome = "YOU LOST.";
            } 
            return (
                <div>
                    <h1>{outcome}</h1>
                </div>
            );
    }
};

/////////////////////////////////////////////////////////
// Get hand function for waiting for the other player. //
/////////////////////////////////////////////////////////

/* eslint-disable */

//<div className="container">
//<p id="xs">ummm</p>
//<p id="os"></p>

exports.GetHandb = class extends React.Component {
    render() {
        const {parent, xs, os} = this.props;
        return (
            <div>
                <audio id="bling">
                    <source src="https://orangefreesounds.com/wp-content/uploads/2021/03/Air-horn-noise.mp3" type="audio/mpeg"></source>
                </audio>
                <div className="container">
                    <center>
                        <img
                            src="https://i.imgur.com/MU4IAQ3.png"
                            width="1100px"
                            height="880px"
                            style={{
                                zIndex: 1,
                                top:"0px",
                                position: "absolute",
                                left: "50%",
                                marginLeft: "-550px",
                            }}
                        ></img>
                    </center>
                    <center>
                        <div
                            style={{
                                zIndex: 2,
                                position: "absolute",
                                top: "170px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >
                            <div className="drop-targets" >
                                <pre>                                  </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box7"
                                    onClick={() => parent.setImg("box7")}
                                    id="box7"
                                ></img>
                                <pre>      </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box8"
                                    onClick={() => parent.setImg("box8")}
                                    id="box8"
                                ></img>
                                <pre>      </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box9"
                                    onClick={() => parent.setImg("box9")}
                                    id="box9"
                                ></img>
                                <pre></pre>
                            </div>
                        </div>
                        <div
                            style={{
                                zIndex: 3,
                                position: "absolute",
                                top: "300px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >


                            <div className="drop-targets" >
                                <pre>                            </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box4"
                                    onClick={() => parent.setImg("box4")}
                                    id="box4"
                                ></img>
                                <pre>        </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box5"
                                    onClick={() => parent.setImg("box5")}
                                    id="box5"
                                ></img>
                                <pre>       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box6"
                                    onClick={() => parent.setImg("box6")}
                                    id="box6"
                                ></img>
                                <pre></pre>
                            </div>
                        </div>
                        <div
                            style={{
                                zIndex: 4,
                                position: "absolute",
                                top: "420px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >


                            <div className="drop-targets" >
                                <pre>                       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box1"
                                    onClick={() => parent.setImg("box1")}
                                    id="box1"
                                ></img>
                                <pre>       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box2"
                                    onClick={() => parent.setImg("box2")}
                                    id="box2"
                                ></img>
                                <pre>        </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box3"
                                    onClick={() => parent.setImg("box3")}
                                    id="box3"
                                ></img>
                                <pre> </pre>
                            </div>
                        </div>
                    </center>
                </div>
            </div>
        );
    }
};

//////////////////////////////
// Get the move of a player //
//////////////////////////////

exports.GetHand = class extends React.Component {
    render() {
        const {parent, xs, os} = this.props;
        return (
            <div style={{top:"0px"}}>
                <audio id="bling">
                    <source src="https://orangefreesounds.com/wp-content/uploads/2021/03/Air-horn-noise.mp3" type="audio/mpeg"></source>
                </audio>
                <div className="container">
                    <center>
                        <img
                            src="https://i.imgur.com/MU4IAQ3.png"
                            width="1100px"
                            height="880px"
                            style={{
                                zIndex: 1,
                                top:"0px",
                                position: "absolute",
                                left: "50%",
                                marginLeft: "-550px",
                            }}
                        ></img>
                    </center>
                    <center>
                        <div
                            style={{
                                zIndex: 2,
                                position: "absolute",
                                top: "170px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >
                            <div className="drop-targets" >
                                <pre>                                  </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box7"
                                    onClick={() => parent.setImg("box7")}
                                    id="box7"
                                ></img>
                                <pre>      </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box8"
                                    onClick={() => parent.setImg("box8")}
                                    id="box8"
                                ></img>
                                <pre>      </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box9"
                                    onClick={() => parent.setImg("box9")}
                                    id="box9"
                                ></img>
                                <pre></pre>
                            </div>
                        </div>
                        <div
                            style={{
                                zIndex: 3,
                                position: "absolute",
                                top: "300px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >


                            <div className="drop-targets" >
                                <pre>                            </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box4"
                                    onClick={() => parent.setImg("box4")}
                                    id="box4"
                                ></img>
                                <pre>        </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box5"
                                    onClick={() => parent.setImg("box5")}
                                    id="box5"
                                ></img>
                                <pre>       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box6"
                                    onClick={() => parent.setImg("box6")}
                                    id="box6"
                                ></img>
                                <pre></pre>
                            </div>
                        </div>
                        <div
                            style={{
                                zIndex: 4,
                                position: "absolute",
                                top: "420px",
                                left: "50%",
                                marginLeft: "-720px",
                            }}
                        >


                            <div className="drop-targets" >
                                <pre>                       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box1"
                                    onClick={() => parent.setImg("box1")}
                                    id="box1"
                                ></img>
                                <pre>       </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box2"
                                    onClick={() => parent.setImg("box2")}
                                    id="box2"
                                ></img>
                                <pre>        </pre>
                                <img
                                    src="https://imgur.com/hFYHBkY.png"
                                    className="box3"
                                    onClick={() => parent.setImg("box3")}
                                    id="box3"
                                ></img>
                                <pre> </pre>
                            </div>
                        </div>
                    </center>
                </div>
                <div
                    style={{
                    zIndex: 10,
                    position: "absolute",
                    top:"660px",
                    left: "50%",
                    marginLeft: "-250px",
                    fontSize:"80px"
                }}
                >Your turn.
                </div>
            </div>
        );
    }
};
/* eslint-disable */

/////////////////////
// Timeout message //
/////////////////////

exports.Timeout = class extends React.Component {
    render() {
        return <div>There's been a timeout. (Someone took too long.)</div>;
    }
};

//////////////////////////
// Export the functions //
//////////////////////////

export default exports;
