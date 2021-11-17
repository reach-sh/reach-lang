const hostname = "http://localhost:3000"

let getProgStates = () => {
  fetch(`${hostname}/states`)
    .then(response => response.json())
    .then(data => console.log(data));
}

let getProgramState = (s) => {
  fetch(`${hostname}/states/${s}`)
    .then(response => response.json())
    .then(data => console.log(data));
}

let getStateActions = (s) => {
  fetch(`${hostname}/states/${s}/actions`)
    .then(response => response.json())
    .then(data => console.log(data));
}

let respondWithVal = (s,a,v) => {
  fetch(`${hostname}/states/${s}/actions/${a}/?data=${v}`)
    .then(response => response.json())
    .then(data => console.log(data));
}

// -- applies an action (with its parameters) and returns a new state
// -- apply :: Id -> Action -> Params -> App Id
// -- apply = undefined
//
// -- returns the children and how they can be derived
// -- children :: Id -> App (M.Map Id [ (Action, Params) ])
// -- children = undefined
//
// -- returns the parent
// -- parent :: Id -> App Id
// -- parent = undefined
//
// -- returns the state
// -- inspect :: Id -> App State
// -- inspect = undefined
