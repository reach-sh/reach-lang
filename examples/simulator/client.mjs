// import fetch from 'node-fetch';

const fetch = require('node-fetch')

const hostname = "http://localhost:3000"

let getStates = () => {
  fetch(`${hostname}/states`)
    .then(response => response.json())
    .then(data => console.log(data));
}

let getStatus = () => {
  fetch(`${hostname}/status`)
    .then(response => response.json())
    .then(data => console.log(data));
}

// let getStateActions = (s) => {
//   fetch(`${hostname}/states/${s}/actions`)
//     .then(response => response.json())
//     .then(data => console.log(data));
// }
//
// let respondWithVal = (s,a,v) => {
//   fetch(`${hostname}/states/${s}/actions/${a}/?data=${v}`)
//     .then(response => response.json())
//     .then(data => console.log(data));
// }

async function interact(method = 'GET', url = '', data = {}) {
  const response = await fetch(url, {
    method: method,
    mode: 'cors',
    cache: 'no-cache',
    credentials: 'same-origin',
    headers: {
      'Content-Type': 'application/json'
    },
    redirect: 'follow',
    referrerPolicy: 'no-referrer',
    body: JSON.stringify(data)
  });
  return response.json();
}

let load = () => {
  interact('POST', `${hostname}/load`, {})
    .then(data => {
      console.log(data);
    });
}

let init = () => {
  interact('POST', `${hostname}/init`, {})
    .then(data => {
      console.log(data);
    });
}

let ping = () => {
  fetch(`${hostname}/ping`)
    .then(response => response.json())
    .then(data => console.log(data))
    .catch(error => {
      console.error('There has been a problem with your fetch operation:', error);
    });
}

ping()
