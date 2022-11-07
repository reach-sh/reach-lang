import styled from 'styled-components'
import ExpandIcon from '../atoms/ExpandIcon'
import CollapseIcon from '../atoms/CollapseIcon'

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
const ExpandOrCollapseButton = styled.button`
    height: 16px;
    background-color: transparent;
    border: none;
    justify-content: center;
    align-self: center;
`

function ExpandOrCollapseIcon ({open, toggle}: {open: boolean, toggle: Function}) {
    const Icon = open ? CollapseIcon : ExpandIcon
    return (<ExpandOrCollapseButton onClick={() => toggle()}><Icon /></ExpandOrCollapseButton>)
}

function SectionHeaderComponent ({sectionOpen, setSectionOpen, title}: {sectionOpen: boolean, setSectionOpen: Function, title: string}){
    return (
        <SectionHeader>
                <SectionTitle>{title}</SectionTitle>
                <ExpandOrCollapseIcon open={sectionOpen} toggle={()=> setSectionOpen(!sectionOpen)}/>
        </SectionHeader>
    )
}

export default SectionHeaderComponent