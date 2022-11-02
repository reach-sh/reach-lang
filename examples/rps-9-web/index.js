import * as AppViews from "./components/App/index.js";
import * as Attacher from "./components/Attacher/index.js";
import * as Deployer from "./components/Deployer/index.js";
import * as PlayerViews from "./components/Player/index.js";
import RenderViews, { renderDOM } from "./views/renderViews";
import ReachContextProvider from "./context/ReachContext";
import './index.css';
import * as backend from './build/index.main.mjs';
import { loadStdlib } from '@reach-sh/stdlib';
const reach = loadStdlib(process.env);

const Views = {
	...AppViews,
	...Attacher,
	...Deployer,
	...PlayerViews,
};

function App() {
	return (
		<div className='App'>
			<RenderViews {...Views} />
		</div>
	);
}

renderDOM(
	<ReachContextProvider>
		<App />
	</ReachContextProvider>
);

export default App;
