import React, { useState } from "react";
import { useReach } from "../../hooks/useReach";

const FundAccount = () => {
	const { user, standardUnit, defaultFundAmt, fundAccount, skipFundAccount } =
		useReach();
	const [amount, setAmount] = useState({ amt: defaultFundAmt });
	return (
		<div>
			<h2>Fund account</h2>
			<br />
			Balance: {user.balance} {standardUnit}
			<hr />
			Would you like to fund your account with additional {standardUnit}?
			<br />
			(This only works on certain devnets)
			<br />
			<input
				type='number'
				placeholder={defaultFundAmt}
				onChange={(e) => setAmount({ amt: e.currentTarget.value })}
			/>
			<button onClick={() => fundAccount(amount)}>Fund Account</button>
			<button onClick={() => skipFundAccount()}>Skip</button>
		</div>
	);
};

export default FundAccount;
