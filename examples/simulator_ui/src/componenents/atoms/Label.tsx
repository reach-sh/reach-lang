import styled from "styled-components";
const StyledP = styled.p`
  color: var(--off-white);
  padding: 4px;

`;
const StyledBox = styled.div`
  background: var(--light-grey);
  height: fit-content;
  width: fit-content;
  border-radius: 2px;

`;
export default function Label({ text }: { text: string }) {
  return (
    <StyledBox>
      <StyledP>{text}</StyledP>
    </StyledBox>
  );
}
