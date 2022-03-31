import styled from "styled-components";
import { ReactElement, useRef, useCallback } from "react";
import cytoscape from "cytoscape";
import CytoscapeComponent from "react-cytoscapejs";
import klay from "cytoscape-klay";
cytoscape.use(klay);
cytoscape.use(require('cytoscape-dom-node'));


const VisualizerContainer = styled.div`
  grid-row-start: 2;
  margin-top: 16.5vh;
  grid-column-start: 2;
  width: 70vw;
  height: 30vh;
  background: var(--dark-bg);
  color: var(--off-white);
  border: 1px solid ;
`;

const styles = [
  {
    selector: "node",
    style: {
      fontSize: "32px",
      minWidth: "24px",
      maxWidth: "120px",
      aspectRatio: "1/1",
    },
  },
];

export default function VisualizerPanel({
  elements,
}: {
  elements: Array<cytoscape.ElementDefinition>;
}): ReactElement {
  const cy = useRef<cytoscape.Core | null>(null);
  const setCytoscape = useCallback(
    (ref: cytoscape.Core) => {
      cy.current = ref;
    },
    [cy]
  );

  return elements ? (
    <VisualizerContainer>
      <CytoscapeComponent
        elements={elements}
        layout={{ name: "klay" }}
        style={{ width: "70vw", height: "37.5vh" }}
        cy={setCytoscape}
        stylesheet={[
          {
            selector: "node",
            style: {
              "border-color": "#FFFFFF",
              "color": "#101010",
              "background-color":(ele) => ele.data("color"),
              "text-valign":"center",
              "text-halign":"center",
              // "hover": (ele) => ele.data('label'),
              content: (ele) => ele.data("id"),
            },
          },
        ]}
      />
    </VisualizerContainer>
  ) : (
    <VisualizerContainer>Initiate Program</VisualizerContainer>
  );
}
