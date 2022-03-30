import styled from "styled-components";

type Selection = any;
const RightPanelContainer = styled.div`
  background: var(--dark-bg);
  width: 40vw;
  height: 62.5vh;
  display: flex;
`;

const NoSelectionText = styled.p`
  align-self: center;
  text-align: center;
  color: var(--off-white);
  margin-left: auto;
  margin-right: auto;
  white-space: nowrap;
`;

function RightPanel({ selection }: { selection: Selection }) {
  return (
    <RightPanelContainer>
      {selection ? (
        <div>{selection}</div>
      ) : (
        <NoSelectionText>Nothing Selected</NoSelectionText>
      )}
    </RightPanelContainer>
  );
}

export default styled(RightPanel)``;
