import Button from '../atoms/Button'
import expand from '../../assets/expand.svg'
import styled from 'styled-components'

const ThisButton = styled(Button)`
    color: #FFFFFF;
    font-size: 14px;
    line-height: 100%;
    margin-left: -1em;
    p { 
        font-size: 14px;
        font-weight: 700;
    }
`

function SessionsDropdown (sessions: any, open: boolean) {
    return (
        <ThisButton label={'Sessions'} icon={<img src={expand}/>}  backgroundColor={'transparent'}></ThisButton>
    )
}

export default SessionsDropdown