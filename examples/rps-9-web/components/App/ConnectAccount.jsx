import React from "react";
import { useReach } from "../../hooks/useReach";

const ConnectAccount = () => {
	const { connecAccount } = useReach();
	return (
		<div>
			Please wait while we connect to your account. If this takes more than a
			few seconds, there may be something wrong.
			<div>
				<button onClick={connecAccount}>Connect Algo Wallet</button>
			</div>
		</div>
	);
};

export default ConnectAccount;
