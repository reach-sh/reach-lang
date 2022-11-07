import styled from "styled-components";

const Input = styled.select`
  width: 156px;
  height: 28px;
  border: 1px solid #3e3e3e;
  box-sizing: border-box;
  filter: drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25));
  border-radius: 4px;
  background-color: transparent;
  color: var(--off-white);
  font-size: 12px;
  line-height: 100%;
`;
const Icon = styled.img`
  margin-left: -1em;
`;

function DropdownInput({
  options,
  value,
  onChange,
}: {
  options: any;
  value: string;
  onChange: Function;
}) {
  return (
       <Input
          placeholder="Session name"
          value={value}
          onChange={(e) => onChange(e.target.value)}
        >
          {!options ? <option>Loading...</option> : options}
        </Input>
  );
}
export default styled(DropdownInput)``;
