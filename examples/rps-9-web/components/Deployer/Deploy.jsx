import React from "react";
import { useReach } from "../../hooks/useReach";

const Deploy = () => {
	const { deploy, wager, standardUnit } = useReach();
	return (
		<div>
			Wager (pay to deploy): <strong>{wager}</strong> {standardUnit}
			<br />
			<button onClick={() => deploy()}>Deploy</button>
		</div>
	);
};

export default Deploy;
