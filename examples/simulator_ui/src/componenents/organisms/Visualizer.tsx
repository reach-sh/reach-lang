import styled from "styled-components";
import { ReactElement, useState, createRef, useEffect } from "react";
import Graphin, { IUserEdge, Graph, Behaviors, IG6GraphEvent, IUserNode, NodeConfig } from "@antv/graphin";
import { ParticipantDropdown } from "../atoms/ParticipantDropdown";
import { Participant } from "../../types";
const {ClickSelect} = Behaviors



type GraphinRef = {
  graph: Graph
  apis: any
}

const VisualizerContainer = styled.div`
  display: grid;
  grid-template-rows: 28px 1fr;
  grid-row-start: 2;
  margin-top: 16.5vh;
  grid-column-start: 2;
  width: 70vw;
  height: 30vh;
  background: var(--dark-bg);
  color: var(--off-white);
  border: 1px solid;
  justify-content: flex-start;
`;

const PerspectiveSelector = styled(ParticipantDropdown)``;

function actorToColorString (actor: number): string {
  switch (actor) {
    case -1:
      return "#204EC5";
    case 0: 
      return "#6AC6E7";  
    default:
      return "#6AC6E7";
  }
}

const defaultNode = { 
  type : 'graphin-circle' , 
  style : { 
    keyshape : { 
      fill : '#6AC6E7' , 
      stroke : '#FFFFFF' , 
      fillOpacity : 1 , 
      size : 30 , 
    } ,
    label : { 
      visible : true , 
    } ,
    icon: {},
    badges: [],
    halo: {
      visible: false
    } 
  } ,
} ;


export default function VisualizerPanel({
  data,
  selectNode,
  participants,
  edges,
  nodes,
  nodeId
}: {
  data: any;
  selectNode: Function;
  participants: Participant[];
  edges: IUserEdge[]
  nodes: any,
  nodeId: number
}): ReactElement {
  const graphinRef = createRef<Graphin>();
  const [perspective, changePerspective] = useState<string>("");
  const isAParticipant = (participant: Participant) => {
    return (participant === participant)
  }

  const SelectNode = ({nodeId}:{ nodeId: number}) => {
  const { graph, apis } = graphinRef.current as GraphinRef;

  useEffect(() => {
    apis.focusNodeById(nodeId.toString());

    const handleClick = (evt: IG6GraphEvent) => {
      const node = evt.item as any;
      const model = node.getModel() as NodeConfig;
      apis.focusNodeById(model.id);
    };


    
    graph.on('node:click', handleClick);
    return () => {
      graph.off('node:click', handleClick);
    };
  }, []);
  return null;
};
  

    if(nodes.length > 0){
      nodes.forEach((node: any) => {
        node.style = {
          keyshape:{
            fill: actorToColorString(node.actor)
          },
          label : {
            value: node.id,
            position: 'center',
            fill: 'white',
            offset: [0, 6]
          },
        }
      })
    }
    if(edges.length > 0){
      edges = edges.map((edge: any):IUserEdge => {
        return {source: edge[0].toString(), target: edge[1].toString()}
      })
    }

    useEffect(() => {
    const {
      graph, // Graph instance of g6
      apis, // API interface provided by Graphin
    } = graphinRef.current as GraphinRef;


  }, []);

  return data ? (
    <VisualizerContainer>
      {participants && (
        <PerspectiveSelector
          participants={participants}
          value={perspective}
          setValue={changePerspective}
          predicate={isAParticipant}
        />
      )}
      <Graphin
        ref={graphinRef}
        defaultNode={defaultNode}
        data={{nodes: nodes, edges: edges}}
        layout={{ type: "dagre", rankdir: "LR" }}
        theme={{
          background: "#101010",
          primaryColor: "#6AC6E7",
          primaryEdgeColor: "#FFFFFF",
          edgeSize: 1,
        }}
      >
        <SelectNode nodeId={nodeId}/>
      </Graphin>
    </VisualizerContainer>
  ) : (
    <VisualizerContainer>Initiate Program</VisualizerContainer>
  );
}
