import { ChangeEventHandler } from "react";
import styled from "styled-components";

const FormContainer = styled.div`
    display: flex ;
    flex-direction: row;
`

const FormItem = styled.div`
    margin-top: 16px;
    margin-bottom: 8px;
    text-align: left;
`
const ItemLabel = styled.label`
    color: var(--off-white);
    margin-bottom: 8px;
`
const InitInput = styled.input.attrs({
  placeholderTextColor: "#FFFFFF"
})`
  background-color: var(--dark-bg);  
  color: var(--off-white);
  border-radius: 4px;
`
function capitalizeFirstLetter(string: string) {
  return string.charAt(0).toUpperCase() + string.slice(1);
}

export default function InitForm({initDetails, updateEntry, initValues}:{initDetails: any, updateEntry: ChangeEventHandler, initValues: any}) {
  return (
    <FormContainer>
        {initDetails && initValues &&
        Object.entries(initDetails).map((entry: any, index: number) => (
            <FormItem key={index}>
                <span>
                    <ItemLabel>{capitalizeFirstLetter(entry[0])}: </ItemLabel> 
                    <ItemLabel>{entry[1].slice(7)}</ItemLabel>
                </span>
                <InitInput
                    placeholder={`${entry[0]} ${entry[1].slice(7)}`}
                    value={initValues[`${entry[0]}`]}
                    onChange={updateEntry}
                />
            </FormItem>
        ))}
    </FormContainer>
  );
}