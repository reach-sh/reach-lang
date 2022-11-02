import React from "react";
import { useReach } from "../../hooks/useReach";

const SetWager = () => {
	const { handleWager, defaultWager, standardUnit, setWager } = useReach();
	return (
		<div>
			<input
				type='number'
				placeholder={defaultWager}
				onChange={(e) => setWager(e.currentTarget.value)}
			/>
			{standardUnit}
			<br />
			<button onClick={() => handleWager()}>Set wager</button>
		</div>
	);
};

export default SetWager;
