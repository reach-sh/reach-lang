import React from "react";
import { useReach } from "../../hooks/useReach";

const Done = () => {
	const { outcome } = useReach();
	return (
		<div>
			Thank you for playing. The outcome of this game was:
			<br />
			{outcome || "Unknown"}
		</div>
	);
};

export default Done;
