import React from "react";

const Wrapper = ({ children }) => {
	return (
		<div className='Attacher'>
			<h2>Attacher (Bob)</h2>
			{children}
		</div>
	);
};
export default Wrapper;
