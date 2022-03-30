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
type Participant = {
  actorId: number;
  nodeId: number;
  actorSet: any;
  apiSet: any;
  who: string;
  phase: string;
};
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
  const translatePhase = (phase: string) => {
    let phaseAsNumber = parseInt(phase);
    switch (phaseAsNumber) {
      case 0:
        return "Program Running";
      case 1:
        return "Initial";
      case 2:
        return "Program finished";
      default:
        return "Phase unknown";
    }
  };
  let participants;
  if (objectViewData == {}) {
    participants = <p>Loading participants...</p>;
  }
  if (Object.entries(objectViewData.objectViewData.obs).length > 0) {
    participants = objectViewData.objectViewData.obs.map(
      (participant: Participant) => {
        return (
          <ParticipantEntry>
            <Left>
              <Who key={participant.actorId}>{participant.who}</Who>
              <Status text={translatePhase(participant.phase)} />
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
