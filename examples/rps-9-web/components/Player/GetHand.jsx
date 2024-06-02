import React from "react";
import { useReach } from "../../hooks/useReach";

const GetHand = () => {
	const { playable, hand, playHand } = useReach();
	return (
		<div>
			{hand ? "It was a draw! Pick again." : ""}
			<br />
			{!playable ? "Please wait..." : ""}
			<br />
			<button disabled={!playable} onClick={() => playHand("ROCK")}>
				Rock
			</button>
			<button disabled={!playable} onClick={() => playHand("PAPER")}>
				Paper
			</button>
			<button disabled={!playable} onClick={() => playHand("SCISSORS")}>
				Scissors
			</button>
		</div>
	);
};

export default GetHand;
