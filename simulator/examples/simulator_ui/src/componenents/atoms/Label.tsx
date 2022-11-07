import styled from "styled-components";
const StyledP = styled.p`
  color: var(--off-white);
  padding: 4px;
  margin: 4px;

  align-self: center;
  justify-self: center;
  font-size: 12px;
  line-height: 150%;
`;
const StyledBox = styled.div`
  background: var(--light-grey);
  height: fit-content;
  width: fit-content;
  border-radius: 2px;
  margin-left: 20px;
`;
export default function Label({ text }: { text: string }) {
  return (
    <StyledBox>
      <StyledP>{text}</StyledP>
    </StyledBox>
  );
}
