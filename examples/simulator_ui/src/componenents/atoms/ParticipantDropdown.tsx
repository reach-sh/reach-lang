import DropdownInput from "./DropdownInput";
import { Participant } from "../../types";
import { useEffect, useState } from "react";
import {filterAsync} from "lodasync"
import Async, { useAsync } from 'react-select/async';

export const ParticipantDropdown = ({
  participants,
  value,
  setValue,
  predicate
}: {
  participants: Participant[];
  value: any;
  setValue: Function;
  predicate: Function;
}) => {

  const [options, setOptions] = useState([])
  useEffect(() => {
      const filterParticipants =  async (unfilteredParticipants: Participant[]): Promise<any> => {
       
      return filterAsync(predicate, unfilteredParticipants)
    }
    const setFilteredOptions = async (p: Participant[]) => {
      const options = await filterParticipants(p)
      setOptions(options)
    }
    setFilteredOptions(participants)
  }, [])

  const createOptions = (participants: Participant[]): JSX.Element[] => {    
    return participants.map((participant) => (
      <option key={participant.actorId} value={participant.actorId}>{participant.who}</option>
    ));
    };
  const nullOption = () => <option key={'nullOpt'} style={{display: "none"}}></option>
  if(participants && participants.length > 0){
    const opt = createOptions(options);    
    return <DropdownInput value={value || ''} options={[nullOption(),...opt]} onChange={setValue} />;
  } else {
      return null
  }
};
