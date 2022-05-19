import Label from "../atoms/Label";
import styled from "styled-components";
import { SectionDivider } from "../atoms/SectionDivider";
import RightArrow from "../atoms/RightArrow";

const MoreArrow = styled(RightArrow)`
  align-self: right;
`;

const DetailContainer = styled.div`
  display: flex;
  flex-direction: row;
  width: 100%;
  align-items: space-between;
  text-align: left;
`;

const DetailLabel = styled(Label)`
  margin-top: 2em;
`;
const Detail = styled.p`
  color: var(--off-white);
  margin-left: 20px;
  width: 90%;
`;
const ViewContainer = styled.div`
  display: flex;
  flex-direction: column;
  width: 100%;
`;
const DetailSectionDivider = styled(SectionDivider)`
  width: 90%;
  align-self: center;
  margin-left: -15px;
`;
export const DetailView = ({
  data,
  apis,
  actions,
}: {
  data: any;
  apis: any;
  actions: any;
}) => {
  const { locals, globals } = data;
  const actorId = parseInt(locals.l_curr_actor_id);
  const actorSet = {} as any;
  for (const [k, v] of Object.entries(locals.l_locals)) {
    const key = k as string;
    const value = v as any;
    const who = value.l_who ? value.l_who : "Consensus";
    actorSet[key] = who;
  }
  const apiSet = {} as any;
  for (const [k, v] of Object.entries(apis)) {
    const value = v as any;
    const who = value.a_name;
    apiSet[k] = who;
  }
  const details = locals.l_locals[actorId];
  const ledger = globals.e_ledger;
  const funds = ledger[actorId][-1];
  let status = "Initial";
  switch (details.l_ks) {
    case "PS_Suspend":
      status = "Running";
      break;
    case "PS_Done":
      status = "Done";
      break;
  }
  const who = details.l_who ? details.l_who : "Consensus";
  // const act = await c.getActions(nodeId, actorId);
  return (
    <ViewContainer>
      <DetailLabel text={"Status"} />
      <DetailContainer>
        <Detail>{status}</Detail>
        <MoreArrow />
      </DetailContainer>
      <DetailSectionDivider />
      <DetailLabel text={"Funds"} />
      <DetailContainer>
        <Detail>{funds}</Detail>
        <MoreArrow />
      </DetailContainer>
      <DetailSectionDivider />
    </ViewContainer>
  );
};
