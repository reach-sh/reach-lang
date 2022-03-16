import * as c from '@reach-sh/simulator-client';
import {useState} from 'react'
// import cytoscape from 'cytoscape';
// import klay from 'cytoscape-klay';
import logo from './logo.svg';
import { createGlobalStyle } from 'styled-components';
import getObjectView from './reach/ObjectView'
import LeftPanel from './componenents/organisms/LeftPanel';
import RightPanel from './componenents/organisms/RightPanel'
import CodePanel from './componenents/organisms/CodePanel';
import './App.css';

// initialize the JSON Log
let jsonLog = [
  ["resetServer"],
  ["load"],
  ["init"]
]

async function resetServer (){
  return await c.resetServer()
}

async function loadReach (){
  return await c.load()
}

async function initReach (){
  return await c.init()
}

async function initParticipant (participant: number){
  console.log(participant)
  return c.initFor( participant, 0, "{}", 0 )
}

async function initApp (){
  await resetServer().catch(reason => console.log(reason))
  await loadReach().catch(reason => console.log(reason))
  await initReach().catch(reason => console.log(reason))
}



export const GlobalStyles = createGlobalStyle`
  html {
    font-family: "Roboto", sans-serif;
    --reach-1: #F45747;
    --reach-2: #6AC6E7;
    --step-1: #204EC5;
    --step-2: #53BC90;
    --off-white: #F2F2F2;
    --white: #FFFFFF;
    --dark-bg: #101010;
    --grey: #3e3e3e;
    --light-grey: #373737;
  }
`;

function App() {
  const [initialized, initialize] = useState(false)
  const [nodeId, setNodeId] = useState(0)
  if(!initialized){
    initialize(true)
    initApp().catch(error => console.log(error))
  }
  const objectViewData = getObjectView({nodeId: `${nodeId}`, c})

  return (
    <div className="App">
    <GlobalStyles />
    <LeftPanel initParticipant={initParticipant}/>
    </div>
  );
}

export default App;
