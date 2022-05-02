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
      const filterParticipants =  async (unfilteredParticipants: Participant[]): any => {
        console.log('unfiltered');
        console.log(unfilteredParticipants);
        console.log(predicate)
      return filterAsync(predicate, unfilteredParticipants)
    }
    const setFilteredOptions = async (p: Participant[]) => {
      console.log(p);
      
      const options = await filterParticipants(p)
      console.log(options);
      
      setOptions(options)
    }
    setFilteredOptions(participants)
  }, [])

  const createOptions = (participants: Participant[]): JSX.Element[] => {
    console.log(participants);
    
    return participants.map((participant) => (
      <option key={participant.actorId} value={participant.actorId}>{participant.who}</option>
    ));
    };
  const nullOption = () => <option style={{display: "none"}}></option>
  if(participants && participants.length > 0){
    console.log(participants)
    const opt = createOptions(options);
    console.log(opt);
    
    return <DropdownInput value={value || ''} options={[nullOption(),...opt]} onChange={setValue} />;
  } else {
      return null
  }
};
