import { ChangeEventHandler } from "react";
import styled from "styled-components";

const FormContainer = styled.div`
    display: flex ;
    flex-direction: row;
`

export default function InitForm({initDetails, updateEntry, initValues}:{initDetails: any, updateEntry: ChangeEventHandler, initValues: any}) {
  return (
    <FormContainer>
        {initDetails && initValues &&
        Object.entries(initDetails).map((entry: any) => (
            <>
                {entry[0]}
                <span>{entry[1].slice(7)}</span>
                <input
                    placeholder={`${entry[0]} ${entry[1].slice(7)}`}
                    value={initValues[`${entry[0]}`]}
                    onChange={updateEntry}
                />
            </>
        ))}
    </FormContainer>
  );
}