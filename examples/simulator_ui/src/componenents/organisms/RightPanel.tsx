import { Selection } from "../../types";
import styled from "styled-components";
import { DetailView } from "../molecules/DetailView";


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

const ActionView = styled((props) => <div>Actions are taking place</div>)``;
const ResponseView = styled((props) => <div>Respond?</div>)``;
type DetailData = {
  locals: any;
  globals: any;
};

const View = ({ view, data, apis, actions }: { view: string; data: DetailData, apis: any, actions: any }) => {
  console.log(view);
  console.log(data);
  if (view === "detail") {
    return <DetailView data={data} apis={{}} actions={{}}/>;
  }
  if (view === "actions") {
    return <ActionView data={data} />;
  } else return <ResponseView data={data} />;
};

function RightPanel({selection, apis, actions}: { selection?: Selection, apis: any, actions: any }) {
  return (
    <RightPanelContainer>
      {selection ? (
        <View view={selection.view} data={selection.data} apis={apis} actions={actions}/>
      ) : (
        <NoSelectionText>Nothing Selected</NoSelectionText>
      )}
    </RightPanelContainer>
  );
}

export default styled(RightPanel)``;
