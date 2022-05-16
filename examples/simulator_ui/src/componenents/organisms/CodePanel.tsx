import styled from "styled-components";
import { CodeBlock } from "react-code-blocks";
import SectionHeaderComponent from "../molecules/SectionHeader";
import { useState } from "react";

const CodeBlockTheme = {
  lineNumberColor: `#F2F2F2`,
  lineNumberBgColor: `#101010`,
  backgroundColor: `#101010`,
  textColor: `#F2F2F2`,
  substringColor: `#F2F2F2`,
  keywordColor: `#F45747`,
  attributeColor: `#ffd700`,
  selectorAttributeColor: `#F45747`,
  docTagColor: `#F2F2F2`,
  nameColor: `#ffa07a`,
  builtInColor: `#6AC6E7`,
  literalColor: `#6AC6E7`,
  bulletColor: `#53BC90`,
  codeColor: `#F2F2F2`,
  additionColor: `#53BC90`,
  regexpColor: `#ffa07a`,
  symbolColor: `#53BC90`,
  variableColor: `#ffa07a`,
  templateVariableColor: `#ffa07a`,
  linkColor: `#6AC6E7`,
  selectorClassColor: `#ffa07a`,
  typeColor: `#6AC6E7`,
  stringColor: `#204EC5`,
  selectorIdColor: `#ffa07a`,
  quoteColor: `#d4d0ab`,
  templateTagColor: `#F2F2F2`,
  deletionColor: `#ffa07a`,
  titleColor: `#00e0e0`,
  sectionColor: `#00e0e0`,
  commentColor: `#d4d0ab`,
  metaKeywordColor: `#F2F2F2`,
  metaColor: `#6AC6E7`,
  functionColor: `#53BC90`,
  numberColor: `#6AC6E7`,
};

const PanelContainer = styled.div`
  grid-column-start: 1;
  grid-row-start: 1;
  display: flex;
  flex-direction: column;
  overflow-y: scroll;
  width: auto;
  height:                           91vh;
  background: var(--dark-bg);
  border: 1px solid var(--off-white);
  padding: 10px;
  top: 79px;
`;

const CodeContainer = styled.div`
  /* height: inherit; */
  overflow-y: scroll;
  text-align: left;
  width: inherit;
  height: 100vh - 78px;

  ::-webkit-scrollbar {
    width: 4px;
    background: var(--off-white);
    border-radius: 64px;
  }
  ::-webkit-scrollbar-track {
    background-color: var(--grey);
  }
`;

const Participants = styled(SectionHeaderComponent)`
  position: sticky;
  border: 1px solid #f2f2f2;
  p {
    height: 16px;
    font-family: "Roboto", sans-serif;
    font-style: normal;
    font-weight: bold;
    font-size: 14px;
    line-height: 16px;
  }
`;

const Participant = styled.p`
  color: var(--off-white);
  font-size: 12px;
  line-height: 14px;
`;

const ParticipantLegendBox = styled.div`
  height: 18px;
  width: 18px;
  background-color: var(--dark-bg);
  filter: ${(props) => props.color};
`;

const ParticipantLegend = styled.div`
  display: flex;
  flex-direction: row;
  align-items: center;
`;

const customStyle = { overflowY: "scroll" };

function ParticipantLegendSquare(color: { color: string }) {
  return <ParticipantLegendBox style={color} />;
}

function CodePanel({ code }: { code: string }) {
  const [openParticipantsSection, setOpenParticipantsSection] = useState(false);
  return (
    <PanelContainer>
      <CodeContainer>
        <CodeBlock
          text={code}
          language={"javascript"}
          showLineNumbers={false}
          theme={CodeBlockTheme}
          customStyle={customStyle}
        />
      </CodeContainer>
      <Participants
        sectionOpen={openParticipantsSection}
        setSectionOpen={setOpenParticipantsSection}
        title={"Participants"}
      />
      {openParticipantsSection ? (
        <ParticipantLegend>
          <Participant>Alice</Participant>
          <ParticipantLegendSquare
            color={
              "filter: invert(20%) sepia(25%) saturate(797%) hue-rotate(167deg) brightness(95%) contrast(96%)"
            }
          />
        </ParticipantLegend>
      ) : (
        <></>
      )}
    </PanelContainer>
  );
}

export default styled(CodePanel)``;
