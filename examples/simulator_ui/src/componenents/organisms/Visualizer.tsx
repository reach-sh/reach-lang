import styled from "styled-components";
import {
  ReactElement,
  useState,
  createRef,
  useEffect,
  useContext,
} from "react";
import Graphin, {
  IUserEdge,
  Graph,
  Behaviors,
  IG6GraphEvent,
  IUserNode,
  NodeConfig,
  GraphinContext,
} from "@antv/graphin";
import { ParticipantDropdown } from "../atoms/ParticipantDropdown";
import { Participant } from "../../types";
import { INode } from "@antv/g6";
const { Hoverable } = Behaviors;

type GraphinRef = {
  graph: Graph;
  apis: any;
};

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

function actorToColorString(actor: number): string {
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
  type: "graphin-circle",
  style: {
    keyshape: {
      fill: "#6AC6E7",
      stroke: "#FFFFFF",
      fillOpacity: 1,
      size: 30,
    },
    label: {
      visible: true,
    },
    icon: {},
    badges: [],
    halo: {
      visible: false,
    },
  },
};

export default function VisualizerPanel({
  data,
  selectNode,
  participants,
  edges,
  nodes,
  nodeId,
  graphinRef
}: {
  data: any;
  selectNode: Function;
  participants: Participant[];
  edges: IUserEdge[];
  nodes: any;
  nodeId: number;
  graphinRef: any
}): ReactElement {

  const [perspective, changePerspective] = useState<string>("-1");
  const isAParticipant = (participant: Participant) => {
    return participant === participant;
  };

  const ShowActionOnHover = () => {
    const { graph, apis } = useContext(GraphinContext);
    useEffect(() => {
      if (graphinRef) {
        const showAction = (evt: IG6GraphEvent) => {
          const edge = evt.item as any;
          const edgeModel = edge.getModel() 
          const tag = nodes.find((node: IUserNode) => node.id == edgeModel.target)
          let label = tag?.label
          console.log(label)          
          graph.updateItem(edge, {
            style: {
              label: {
                value: `${label}`,
                fill: "#000000",
                fontSize: 12,
                background: { width: 99, height: 36, radius: 16 },
                offset: [0, -44],
                visible: true,
              },
            },
          });
        };
        const hideAction = (evt: IG6GraphEvent) => {
          const edge = evt.item as any;
          console.log(nodes);
          graph.updateItem(edge, {
            style: {
              label: {
                visible: false,
              },
            },
          });
        }

        graph.on("edge:mouseenter", showAction);
        graph.on("edge:mouseleave", hideAction)
        return () => {
          graph.off("edge:mousenter", showAction);
        };
      }
    }, []);
    return null;
  };

  if (nodes.length > 0) {
    nodes.forEach((node: any) => {
      node.style = {
        keyshape: {
          fill: actorToColorString(node.actor),
        },
        label: {
          value: node.id,
          position: "center",
          fill: "white",
          offset: [0, 6],
        },
      };
    });
  }
  if (edges.length > 0) {
    edges = edges.map(
      (edge: any): IUserEdge => {
        edge.style = {
          color: "#000000",
        };
        return {
          source: edge[0].toString(),
          target: edge[1].toString(),
          style: edge.style,
        };
      }
    );
  }

  useEffect(() => {
    const {
      graph, // Graph instance of g6
      apis, // API interface provided by Graphin
    } = graphinRef.current as GraphinRef;
    console.log('painting now')
    graph.paint()

  }, []);

  const SelectNode = ({ nodeId }: { nodeId: number }) => {
    const { graph, apis } = graphinRef.current as GraphinRef;

    useEffect(() => {
      apis.focusNodeById(nodeId.toString());

      const handleClick = (evt: IG6GraphEvent) => {
        const node = evt.item as INode;
        const model = node.getModel() as NodeConfig;
        apis.focusNodeById(model.id);
      };

      graph.on("node:click", handleClick);
      return () => {
        graph.off("node:click", handleClick);
      };
    }, []);
    return null;
  };
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
        data={{ nodes: nodes, edges: edges }}
        layout={{ type: "dagre", rankdir: "LR" }}
        theme={{
          background: "#101010",
          primaryColor: "#6AC6E7",
          primaryEdgeColor: "#FFFFFF",
          edgeSize: 1,
        }}
      >
        <ShowActionOnHover />
      </Graphin>
    </VisualizerContainer>
  ) : (
    <VisualizerContainer>Initiate Program</VisualizerContainer>
  );
}
