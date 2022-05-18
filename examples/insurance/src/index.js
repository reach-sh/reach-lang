import React from "react";
import ReactDOM from "react-dom";
import './index.css';
import App from "./App";
import { ConfirmContextProvider } from "./store/ConfirmContextProvider";
import ConfirmDialog from "./components/ConfirmDialog";
import reportWebVitals from './reportWebVitals';

ReactDOM.render(
  <React.StrictMode>
    <ConfirmContextProvider>
      <App />
      <ConfirmDialog />
    </ConfirmContextProvider>
  </React.StrictMode>,
  document.getElementById("root")
);

// If you want to start measuring performance in your app, pass a function
// to log results (for example: reportWebVitals(console.log))
// or send to an analytics endpoint. Learn more: https://bit.ly/CRA-vitals
reportWebVitals();
