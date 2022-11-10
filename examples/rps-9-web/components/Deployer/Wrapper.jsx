import React from "react";

const Wrapper = ({ children }) => {
	return (
		<div className='Deployer'>
			<h2>Deployer (Alice)</h2>
			{children}
		</div>
	);
};

export default Wrapper;
