import * as c from "@reach-sh/simulator-client";
import { useEffect, useState } from "react";
// import cytoscape from 'cytoscape';
// import klay from 'cytoscape-klay';
import { createGlobalStyle } from "styled-components";
import getObjectView from "./reach/ObjectView";
import LeftPanel from "./componenents/organisms/LeftPanel";
import RightPanel from "./componenents/organisms/RightPanel";
import Navbar from "./componenents/molecules/Navbar";
import CodePanel from "./componenents/organisms/CodePanel";
import "./App.css";
import styled from "styled-components";

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
// initialize the JSON Log
let jsonLog = [["resetServer"], ["load"], ["init"]];

async function resetServer() {
  return await c.resetServer();
}

async function loadProgram() {
  return await c.load();
}

async function initReach() {
  return await c.init();
}

async function initParticipant(participant: number) {
  console.log(participant);
  return c.initFor(participant, 0, "{}");
}

async function initApp(mountProgram: Function) {
  await resetServer();
  const program = await loadProgram();
  mountProgram(program);
  return await initReach();
}

const Container = styled.div`
  display: flex;
  flex-direction: column;
`;

const AppContainer = styled.div`
  display: flex;
  flex-direction: row;
`;

const InfoContainer = styled.div`
  display: flex;
  flex-direction: column;
`;

function App() {
  const [initialized, initialize] = useState(false);
  const [nodeId, setNodeId] = useState(0);
  const [objectViewData, setObjectViewData] = useState({});
  const [code, mountProgram] = useState("");
  const [jsonLog, updateJsonLog] = useState([
    ["resetServer"],
    ["load"],
    ["init"],
  ]);

  useEffect(() => {
    const fetchObjectViewData = async () => {
      const ovd = await getObjectView({ nodeId: `${nodeId}`, c });
      setObjectViewData(ovd);
    };
    if (!initialized) {
      const initilializeApp = async () => {
        const result = await initApp(mountProgram);
        console.log(result);
        initialize(result === "OK" ? true : false);
      };
      initilializeApp();
    }
    if (initialized && Object.entries(objectViewData).length < 1) {
      fetchObjectViewData();
    }
  });

  useEffect(() => {}, []);

  return (
    <div className="App">
      <GlobalStyles />
      <Container>
        <Navbar />
        <AppContainer>
          <CodePanel code={code} />
          <InfoContainer>
            <LeftPanel
              initParticipant={initParticipant}
              objectViewData={objectViewData}
            />
          </InfoContainer>
        </AppContainer>
      </Container>
    </div>
  );
}

export default App;
