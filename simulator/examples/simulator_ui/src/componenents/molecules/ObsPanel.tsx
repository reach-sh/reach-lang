import Label from "../atoms/Label";
import styled from "styled-components";
import RightArrow from "../atoms/RightArrow";

const Status = styled(Label)`
  margin-left: 20px;
  &&p {
    line-height: 150%;
    font-size: 0.75em;
  }
`;

const Who = styled.p`
  color: var(--off-white);
  margin-block-start: 0.5em;
  margin-block-end: 0.5em;
`;

const ParticipantContainer = styled.div`
  display: flex;
  flex-direction: column;
`;

const ParticipantEntry = styled.span`
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: center;
`;

const DetailArrow = styled(RightArrow)`
  align-self: right;
`;

const Right = styled.div`
  display: flex;
  flex-direction: row;
`;
const Left = styled.div`
  display: flex;
  flex-direction: row;
`;

export default function (objectViewData: any) {
  const translatePhase = (lks: any) => {
    let status = ''
    switch (lks) {
      case 'PS_Suspend':
        status = 'Program Running'
        break;
      case 'PS_Done':
        status = 'Program Done'
        break;
      case null :
        status = 'Uninitialized'
        break;
    };
    return status
  }
  let participants;
  if (objectViewData == {}) {
    participants = <p>Loading participants...</p>;
  }
  if (Object.entries(objectViewData).length > 0) {
    const locals = objectViewData.objectViewData.locals.l_locals
    participants = Object.keys(locals).map(
      (key) => {
        const participant = locals[key]
        return (
          <ParticipantEntry>
            <Left>
              <Who key={ key }>{participant.l_who ? participant.l_who : 'Consensus'}</Who>
              <Status text={participant.l_ks ? translatePhase(participant.l_ks): 'Uninitialized'} />
            </Left>
            <Right>
              <DetailArrow />
            </Right>
          </ParticipantEntry>
        );
      }
    );
  }
  return <ParticipantContainer> {participants} </ParticipantContainer>;
}
