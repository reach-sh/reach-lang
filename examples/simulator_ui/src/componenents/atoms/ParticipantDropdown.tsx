import DropdownInput from "./DropdownInput";
import { Participant } from "../../types";

export const ParticipantDropdown = ({
  participants,
  value,
  setValue,
}: {
  participants: Participant[];
  value: any;
  setValue: Function;
}) => {
  const createOptions = (participants: Participant[]): JSX.Element[] => {
    return participants.map((participant) => (
      <option key={participant.actorId} value={participant.actorId}>{participant.who}</option>
    ));
    };
  if(participants && participants.length > 0){
    const options = createOptions(participants);
    return <DropdownInput value={value || ''} options={options} onChange={setValue} />;
  } else {
      return null
  }
};
