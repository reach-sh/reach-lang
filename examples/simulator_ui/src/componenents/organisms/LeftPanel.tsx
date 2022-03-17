import styled from 'styled-components'
import Button from '../atoms/Button'
import SectionHeaderComponent from '../molecules/SectionHeader'
import { useState } from 'react'
import DropdownInput from '../atoms/DropdownInput'
import ObsPanel from '../molecules/ObsPanel'

const  PanelContainer = styled.div`
    position: static;
    width: 420px;
    height: 562px;
    border: 1px solid var(--off-white);
    box-sizing: border-box;
    background: var(--dark-bg);
`
const StyledPlus = styled(Button)`
    background-color: var(--reach-2);
    label{
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


`
function PlusButton (){
    return (<StyledPlus label="+" icon={<></>} color={'#101010'} ></StyledPlus>)
}

const SectionHeader = styled.div`
    display: flex;
    flex-direction: row;
    justify-content: space-between;
`

const SectionTitle = styled.p`
    color: var(--off-white);
    font-style: normal;
    font-weight: bold;
    font-size: 14px;
    line-height: 16px;

`

const ObjectSection = styled.div`
    margin: 0 7.5%;
    align-self: center;
    align-items: left;
    
`
const ObjectContent = styled.div`
    height: 110px;
`

const AccountsSection = styled.div`
    margin: 0 7.5%;
`
const AccountContent = styled.div`
    max-height: 100px;
`

const AddItem = styled.div`
    display: flex;
    flex-direction: row;
    p{
        color: var(--white);
    };
    align-items: center;
    justify-content: space-between;
`

const InitSection = styled.div`
    margin: 0 7.5%;
`

const InitContent = styled.div`
    height: 184px;
    position: relative;
    display: flex;
    flex-direction: row;
`



const SectionDivider = styled.div`
    border: 0.5px solid var(--white);
    height: 0px;
    width: 85%;
    margin-left: 7.5%;
    align-self: center;
`
const InitParticipantsButton = styled(Button)`
    margin-left: auto;
    width: 156px;
    height: 28px;
`
const ParticipantName = styled.p``




function LeftPanel ({initParticipant, objectViewData}: {initParticipant: Function, objectViewData: any}) {
    const [openObjectSection, setOpenObjectSection] = useState(true);
    const [openAccountsSection, setOpenAccountsSection] = useState(true);
    const [openInitSection, setOpenInitSection] = useState(true);
    const [dropDownSelection, changeDropDownSelection] = useState<number | null>(null)
    const participantChange = (participant: number) => {
    changeDropDownSelection(participant);
  };
    return (
    <PanelContainer>
        <ObjectSection>
            <SectionHeaderComponent sectionOpen={openObjectSection} setSectionOpen={setOpenObjectSection} title='Object'/>
            {openObjectSection && Object.entries(objectViewData).length > 0 ?<ObjectContent>
                <ObsPanel objectViewData={objectViewData}/>
            </ObjectContent>:<></>}
        </ObjectSection>
        <SectionDivider/>
        <AccountsSection>
            <SectionHeaderComponent sectionOpen={openAccountsSection} setSectionOpen={(setOpenAccountsSection)} title={'Accounts/Tokens'} />
            {openAccountsSection ? <AccountContent>
            <AddItem>
            <p>New Account</p>
            <PlusButton />
            </AddItem>
            <AddItem>
                <p>New Token</p>
                <PlusButton />
            </AddItem>

        </AccountContent> : <></>}
        </AccountsSection>
        <SectionDivider/>
        <InitSection>
            <SectionHeaderComponent sectionOpen={openInitSection} setSectionOpen={setOpenInitSection} title='Participant Initialization'/>
            {openInitSection?<InitContent>
                <DropdownInput value={dropDownSelection ? dropDownSelection.toString() : ''} onChange={participantChange}/>
            <InitParticipantsButton label='Init Participant' icon={<></>} onClick={ () =>{console.log(dropDownSelection); return initParticipant(dropDownSelection)}}  />
            </InitContent>:<></>}
        </InitSection>
    </PanelContainer>
    )
}

export default styled(LeftPanel)`

`