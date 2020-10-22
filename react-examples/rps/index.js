import React from 'react';
import ReactDOM from 'react-dom';
import * as AppViews from './views/AppViews';

// import './index.css';
// import * as backend from './build/index.main.mjs';
// import * as reach from '@reach-sh/stdlib/ETH';

function renderDOM() {
  ReactDOM.render(
    <React.StrictMode><App /></React.StrictMode>,
    document.getElementById('root')
  );
}

class App extends React.Component {
  render() {
    return <AppViews.Hello />;
  }
}

renderDOM();
