import logo from './logo.svg';
import './App.css';
import * as reachsdk from '@reach-sh/stdlib';
const reach = reachsdk.loadStdlib({REACH_CONNECTOR_MODE: 'ETH'});
const rand = reach.randomUInt().toString();
// import * as reach from '@reach-sh/stdlib/ETH';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <img src={logo} className="App-logo" alt="logo" />
        <p>
          {rand}
        </p>
        <p>
          Edit <code>src/App.js</code> and save to reload.
        </p>
        <a
          className="App-link"
          href="https://reactjs.org"
          target="_blank"
          rel="noopener noreferrer"
        >
          Learn React
        </a>
      </header>
    </div>
  );
}

export default App;
