import React, { useState } from "react";
import { useReach } from "../../hooks/useReach";

const AcceptTerms = () => {
	const [disabled, setDisabled] = useState(false);
	const { wager, standardUnit, termsAccepted } = useReach();
	return (
		<div>
			The terms of the game are:
			<br /> Wager: {wager} {standardUnit}
			<br />
			<button
				disabled={disabled}
				onClick={() => {
					setDisabled(true);
					termsAccepted();
				}}
			>
				Accept terms and pay wager
			</button>
		</div>
	);
};

export default AcceptTerms;
