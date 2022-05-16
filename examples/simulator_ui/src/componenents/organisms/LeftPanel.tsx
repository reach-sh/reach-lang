import styled from "styled-components";
import Button from "../atoms/Button";
import SectionHeaderComponent from "../molecules/SectionHeader";
import { useState, useEffect } from "react";
import ObsPanel from "../molecules/ObsPanel";
import { ParticipantDropdown } from "../atoms/ParticipantDropdown";
import InitForm from "../molecules/InitForm";
import { Participant } from "../../types";


const PanelContainer = styled.div`
  position: static;
  width: 30vw;
  height: 62.5vh;
  border: 1px solid var(--off-white);
  box-sizing: border-box;
  background: var(--dark-bg);
  grid-column-start: 3;
`;
const StyledPlus = styled(Button)`
  background-color: var(--reach-2);
  label {
    font-style: normal;
    font-weight: normal;
    font-size: 12px;
    color: var(--dark-bg);
  }
  line-height: 150%;
  border-radius: 2px;
  width: 38px;
  height: 26px;
  text-align: center;
`;
function PlusButton() {
  return <StyledPlus label="+" icon={<></>} color={"#101010"}></StyledPlus>;
}

const SectionHeader = styled.div`
  display: flex;
  flex-direction: row;
  justify-content: space-between;
`;

const SectionTitle = styled.p`
  color: var(--off-white);
  font-style: normal;
  font-weight: bold;
  font-size: 14px;
  line-height: 16px;
`;

const ObjectSection = styled.div`
  margin: 0 7.5%;
  align-self: center;
  align-items: left;
`;
const ObjectContent = styled.div`
  height: 110px;
`;

const AccountsSection = styled.div`
  margin: 0 7.5%;
`;
const AccountContent = styled.div`
  max-height: 100px;
`;

const AddItem = styled.div`
  display: flex;
  flex-direction: row;
  p {
    color: var(--white);
  }
  align-items: center;
  justify-content: space-between;
`;

const InitSection = styled.div`
  margin: 0 7.5%;
`;

const InitContent = styled.div`
  height: 184px;
  position: relative;
  display: flex;
  flex-direction: column;
  align-items: space-between;
`;

const SectionDivider = styled.div`
  border: 0.5px solid var(--white);
  height: 0px;
  width: 85%;
  margin-left: 7.5%;
  align-self: center;
`;
const InitParticipantsButton = styled(Button)`
  margin-left: auto;
  width: 156px;
  height: 28px;
`;
const ParticipantName = styled.p``;



function LeftPanel({
  initParticipant,
  objectViewData,
  participants,
  getInitDetails,
}: {
  getInitDetails: Function;
  initParticipant: Function;
  objectViewData: any;
  participants: any;
}) {
  const [openObjectSection, setOpenObjectSection] = useState(true);
  const [openAccountsSection, setOpenAccountsSection] = useState(true);
  const [openInitSection, setOpenInitSection] = useState(true);
  const [dropDownSelection, changeDropDownSelection] = useState<number>();
  const [initDetails, setInitDetails] = useState<any>();
  const [initValues, setInitValues] = useState<any>({});
  const updateEntry = (e: any) => {
    const ph = Object.entries(initDetails).filter(
      (detail) => detail[0] === e.target.placeholder.split(" ")[0]
    );
    const placeholder = ph as Array<any>;
    const type = placeholder[0][1].slice(7);
    const value =
      type === "UInt" ? parseInt(e.target.value) : `${e.target.value}`;
    setInitValues({ ...initValues, [`${ph[0][0]}`]: value });
  };
  const participantHasDetails = async (participant: Participant) => {
        const details = await getInitDetails(participant.actorId)
        return Object.entries(details).length > 0
      }


  useEffect(() => {
    const setDetails = async () => {
      const details = await getInitDetails(dropDownSelection);
      setInitDetails(details);
    };
    setDetails();
  }, [dropDownSelection]);

  return (
    <PanelContainer>
      <ObjectSection>
        <SectionHeaderComponent
          sectionOpen={openObjectSection}
          setSectionOpen={setOpenObjectSection}
          title={`Object (Node ${objectViewData.nodeId})`}
        />
        {openObjectSection && Object.entries(objectViewData).length > 0 ? (
          <ObjectContent>
            <ObsPanel objectViewData={objectViewData} />
          </ObjectContent>
        ) : (
          <></>
        )}
      </ObjectSection>
      <SectionDivider />
      <AccountsSection>
        <SectionHeaderComponent
          sectionOpen={openAccountsSection}
          setSectionOpen={setOpenAccountsSection}
          title={"Accounts/Tokens"}
        />
        {openAccountsSection ? (
          <AccountContent>
            <AddItem>
              <p>New Account</p>
              <PlusButton />
            </AddItem>
            <AddItem>
              <p>New Token</p>
              <PlusButton />
            </AddItem>
          </AccountContent>
        ) : (
          <></>
        )}
      </AccountsSection>
      <SectionDivider />
      <InitSection>
        <SectionHeaderComponent
          sectionOpen={openInitSection}
          setSectionOpen={setOpenInitSection}
          title="Participant Initialization"
        />
        {openInitSection ? (
          <InitContent>
            {objectViewData &&(
              <ParticipantDropdown
                participants={participants}
                setValue={changeDropDownSelection}
                value={dropDownSelection}
                predicate={participantHasDetails}
              />
            )}
            {initDetails && <InitForm
              initDetails={initDetails}
              updateEntry={updateEntry}
              initValues={initValues}
            />}
            {initDetails && <InitParticipantsButton
              label="Init Participant"
              icon={<></>}
              onClick={() => {
                return initParticipant({
                  participant: dropDownSelection,
                  node: objectViewData.nodeId,
                  values: initValues,
                  details: initDetails
              });
              }}
            />}
          </InitContent>
        ) : (
          <></>
        )}
      </InitSection>
    </PanelContainer>
  );
}

export default styled(LeftPanel)``;
