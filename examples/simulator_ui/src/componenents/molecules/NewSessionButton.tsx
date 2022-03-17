import NewSessionIcon from "../atoms/NewSessionIcon";
import styled from 'styled-components'
import Button from '../atoms/Button'

const NewSessionBtn = styled(Button)`
    display: flex;
    flex-direction: row;
    justify-content: center;
    align-items: center;
    padding: 8px 24px;
    background: var(--white);
    margin-left: 6em;
    height: 30px;
    & p {
        color: var(--dark-bg);
        white-space: nowrap;
        font-family: "Roboto" sans-serif;
        font-style: normal;
        font-size: 14px;
        line-height: 100%;
        flex: none;
        order: 0;
        flex-grow: 1;
        margin: 0px 1em;
        margin-left: -.25em;
    }
    img {
        margin-left: 2.5em;
        width: 1em;
        height: 1.2em;
    }

`

function NewSessionButton (){
    return (
        <NewSessionBtn label="Start new session" icon={<NewSessionIcon />} color='101010'/>
    )
}

export default NewSessionButton