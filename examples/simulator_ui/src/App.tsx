import * as c from "@reach-sh/simulator-client";
import { useEffect, useState } from "react";
import { createGlobalStyle } from "styled-components";
import getObjectView from "./reach/ObjectView";
import LeftPanel from "./componenents/organisms/LeftPanel";
import RightPanel from "./componenents/organisms/RightPanel";
import Navbar from "./componenents/molecules/Navbar";
import CodePanel from "./componenents/organisms/CodePanel";
import "./App.css";
import styled from "styled-components";
import VisualizerPanel from "./componenents/organisms/Visualizer";
import { IUserNode, IUserEdge } from "@antv/graphin";
import { ActorSet, Participant, Actor } from "./types";

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

async function getInitDetails(a: number){
  return await c.initDetails(a)
}

async function initParticipant(participant: number, nodeId: number, iface: any) {
  const values = JSON.stringify(iface)
  console.log(values)
  return c.initFor(participant, nodeId, values);
}

async function initApp(mountProgram: Function) {
  await resetServer();
  const program = await loadProgram();
  mountProgram(program);
  return await initReach();
}

const getParticipants = (
  objectViewData: any,
  nodeId: number
): Participant[] => {
  const p = [];
  if (Object.keys(objectViewData).length > 1) {
    for (const [key, value] of Object.entries(objectViewData.locals.l_locals)) {
      const value2 = value as any;
      const who = value2.l_who ? value2.l_who : "Consensus";
      p.push({
        actorId: parseInt(key),
        nodeId: nodeId,
        actorSet: `${JSON.stringify(objectViewData.locals.l_locals)}`,
        apiSet: `${JSON.stringify(objectViewData.apis)}`,
        who: `${who}`,
        phase: `${value2.l_phase}`,
      });
    }
  }

  return p;
};

const Container = styled.div`
  display: flex;
  flex-direction: column;
  width: 100vw;
  height: 100vh;
  overflow-x: hidden;
  overflow-y: hidden;
`;

const InfoContainer = styled.div`
  display: grid;
  grid-template-columns: 30% 30% 40%;
  grid-template-rows: 33% 33% 33%;
`;

const Info = styled.div`
  display: flex;
  flex-direction: row;
  grid-column-start: 2;
  width: 70vw;
  height: 62.5vh;
`;

function App() {
  const [initialized, initialize] = useState(false);
  const [nodeId, setNodeId] = useState(0);
  const [objectViewData, setObjectViewData] = useState<any>({});
  const [code, mountProgram] = useState("");
  const [nodes, setGraphNodes] = useState<IUserNode[]>([]);
  const [edges, setGraphEdges] = useState<IUserEdge[]>([]);
  const [jsonLog, updateJsonLog] = useState([
    ["resetServer"],
    ["load"],
    ["init"],
  ]);

  useEffect(() => {
    const getVisualizerData = async () => {
      const n = await c.getStates();
      console.log(n)
      if (nodes.length < Object.keys(n).length) {
        let nodeArray = [];
        for (const [s, dets] of Object.entries(n)) {
          const details = dets as any;
          let displayLabel = details[1].tag.slice(2);
          let actorStateID = details[0];
          if (displayLabel !== "None") {
            displayLabel = displayLabel + "?";
          }
          nodeArray.push({ id: s, label: displayLabel, actor: actorStateID });
          console.log(nodeArray)
          setGraphNodes(nodeArray);
          console.log(nodes);
        }
        const e = await c.getEdges();
        setGraphEdges(e);
      }
    };
    getVisualizerData();
  }, []);

  useEffect(() => {
    const fetchObjectViewData = async () => {
      const ovd = await getObjectView({ nodeId: `${nodeId}`, c });
      setObjectViewData(ovd);
    };
    if (!initialized) {
      const initilializeApp = async () => {
        const result = await initApp(mountProgram);
        initialize(result === "OK" ? true : false);
        if (Object.entries(objectViewData).length < 1) {
          fetchObjectViewData();
        }
      };
      initilializeApp();
    }
    if (initialized && Object.entries(objectViewData).length < 1) {
      fetchObjectViewData();
    }
  }, []);

  useEffect(() => {
    console.log(getParticipants(objectViewData, nodeId));
  }, []);

  return (
    <div className="App">
      <GlobalStyles />
      <Container>
        <Navbar />
        <InfoContainer>
          <CodePanel code={code} />
          <Info>
            <LeftPanel
              initParticipant={initParticipant}
              getInitDetails={getInitDetails}
              objectViewData={objectViewData}
              participants={
                objectViewData ? getParticipants(objectViewData, nodeId) : []
              }
            />
            <RightPanel selection={null} />
          </Info>
          <VisualizerPanel
            data={objectViewData}
            nodes={nodes}
            edges={edges}
            selectNode={setNodeId}
            participants={
              objectViewData ? getParticipants(objectViewData, nodeId) : []
            }
          />
        </InfoContainer>
      </Container>
    </div>
  );
}

export default App;
