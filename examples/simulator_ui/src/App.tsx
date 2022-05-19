import * as c from "@reach-sh/simulator-client";
import { useEffect, useState, useReducer, createRef } from "react";
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
import { Participant } from "./types";
import Graphin from "@antv/graphin";
import { Selection } from "./types";

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
async function resetServer() {
  return await c.resetServer();
}

async function loadProgram() {
  return await c.load();
}

async function initReach() {
  return await c.init();
}

async function getInitDetails(a: number) {
  return await c.initDetails(a);
}

async function initParticipant(
  participant: number,
  nodeId: number,
  iface: any,
  details: any
) {
  const values = {} as any;
  for (const [k, v] of Object.entries(iface)) {
    values[k] = { tag: `V_${details[k].slice(7)}`, contents: v };
  }
  return [await c.initFor(participant, nodeId, values), values];
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
  const graphinRef = createRef<Graphin>();
  const [initialized, initialize] = useState(false);
  const [nodeId, setNodeId] = useState(0);
  const [objectViewData, setObjectViewData] = useState<any>({});
  const [code, mountProgram] = useState("");
  const [nodes, setGraphNodes] = useState<IUserNode[]>([]);
  const [edges, setGraphEdges] = useState<IUserEdge[]>([]);
  const [_, forceUpdate] = useReducer((x) => x + 1, 0);
  const [selection, setSelection] = useState<Selection>();
  const [locals, setLocals] = useState<any>();
  const [globals, setGlobals] = useState<any>();
  const [perspective, changePerspective] = useState<string>("-1");
  const [actions, setActions] = useState<any>();
  const [apis, setApis] = useState<any>();
  const [jsonLog, updateJsonLog] = useState<any>([
    ["resetServer"],
    ["load"],
    ["init"],
  ]);

  const initLogAndUpdate = async ({
    participant,
    node,
    values,
    details,
  }: {
    participant: number;
    node: number;
    values: any;
    details: any;
  }) => {
    let result = await initParticipant(participant, node, values, details);
    if (result[0] == "OK") {
      setNodeId(nodeId + 1);
      updateJsonLog([
        ...jsonLog,
        ["initFor", node, participant, JSON.stringify(result[1])],
      ]);
      forceUpdate();
    }
  };

  useEffect(() => {
    const getVisualizerData = async () => {
      const g = await c.getStateGlobals(nodeId);
      setGlobals(g)
      const l = await c.getStateLocals(nodeId);
      setLocals(l)
      const n = await c.getStates();
      if (nodes.length < Object.keys(n).length) {
        let nodeArray = [];
        for (const [s, dets] of Object.entries(n)) {
          const details = dets as any;
          let displayLabel = details[1].tag.slice(2);
          let actorStateID = details[0];
          nodeArray.push({ id: s, label: displayLabel, actor: actorStateID });
          setGraphNodes(nodeArray);
        }
        const e = await c.getEdges();
        setGraphEdges(e);
      }
      if(nodeId && perspective){
        let a = await c.getActions(nodeId, parseInt(perspective))
        setActions(a)
        let apis = await c.getAPIs()
        setApis(apis)
      }
    };
    getVisualizerData();
  }, [nodeId, jsonLog]);

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
  }, [jsonLog]);
  return (
    <div className="App">
      <GlobalStyles />
      <Container>
        <Navbar />
        <InfoContainer>
          <CodePanel code={code} />
          <Info>
            <LeftPanel
              initParticipant={initLogAndUpdate}
              getInitDetails={getInitDetails}
              objectViewData={objectViewData}
              participants={
                objectViewData && getParticipants(objectViewData, nodeId)
              }
            />
            <RightPanel selection={selection} apis={apis} actions={actions} />
          </Info>
          <VisualizerPanel
            graphinRef={graphinRef}
            data={objectViewData}
            nodes={nodes}
            edges={edges}
            locals={locals}
            globals={globals}
            selectNode={setSelection}
            perspective={perspective}
            changePerspective={changePerspective}
            participants={
              objectViewData && getParticipants(objectViewData, nodeId)
            }
            nodeId={nodeId}
          />
        </InfoContainer>
      </Container>
    </div>
  );
}

export default App;
