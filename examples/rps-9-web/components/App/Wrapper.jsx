import React from "react";

const Wrapper = ({ children }) => {
	return (
		<div>
			<div className='App'>
				<header className='App-header' id='root'>
					<h1>Rock, Paper, Scissors</h1>
					{children}
				</header>
			</div>
		</div>
	);
};

export default Wrapper;
