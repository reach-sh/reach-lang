import styled from "styled-components";
import { ReactElement, useState, useEffect } from "react";
import Graphin, { GraphinData, IUserEdge, IUserNode } from "@antv/graphin";
import { ParticipantDropdown } from "../atoms/ParticipantDropdown";
import { Participant } from "../../types";

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
  nodes
}: {
  data: any;
  selectNode: Function;
  participants: Participant[];
  edges: IUserEdge[]
  nodes: any
}): ReactElement {
  const [perspective, changePerspective] = useState<string>("");
    if(nodes.length > 0){
      nodes.forEach((node: any) => {
        console.log('node');
        console.log(node)
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
  

  return data ? (
    <VisualizerContainer>
      {participants && (
        <PerspectiveSelector
          participants={participants}
          value={perspective}
          setValue={changePerspective}
        />
      )}
      <Graphin
        defaultNode={defaultNode}
        data={{nodes: nodes, edges: edges}}
        layout={{ type: "dagre", rankdir: "RL" }}
        theme={{
          background: "#101010",
          primaryColor: "#6AC6E7",
          primaryEdgeColor: "#FFFFFF",
          edgeSize: 1,
        }}
      />
    </VisualizerContainer>
  ) : (
    <VisualizerContainer>Initiate Program</VisualizerContainer>
  );
}
