import Label from "../atoms/Label";
import styled from "styled-components";
const Status = styled(Label)``;
type Participant = {
  actorId: number;
  nodeId: number;
  actorSet: any;
  apiSet: any;
  who: string;
};
const Who = styled.p`
  color: var(--off-white);
`;

const ParticipantContainer = styled.div`
  align-items: start;
`

export default function (objectViewData: any) {
  let participants;
  if (objectViewData == {}) {
    participants = <p>Loading participants...</p>;
  }
  if (Object.entries(objectViewData.objectViewData.obs).length > 0) {
    participants = objectViewData.objectViewData.obs.map(
      (participant: Participant) => {
        return (
            <Who key={participant.actorId}>{participant.who}</Who>
        );
      }
    );
  }
  return <ParticipantContainer> {participants} </ParticipantContainer>;
}
