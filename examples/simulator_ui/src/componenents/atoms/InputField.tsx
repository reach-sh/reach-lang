import styled from 'styled-components'
import editIcon from '../../assets/edit_icon.svg'

const Input = styled.input`
    outline: none;
    background: transparent;
    border-top-style: hidden;
    border-right-style: hidden;
    border-left-style: hidden;
    border-bottom-style: groove;
    border-color: var(--reach-1);
    padding: 10px;
    padding-left: 0px;
    color: var(--off-white);
    margin-left: 2em;

    ::placeholder {
        color: var(--off-white);
        font-size: 14px;
        line-height: 16px;
        font-weight: bold;
        align-self: flex-start;
        /* margin-left: -5em; */
}
`
const Icon = styled.img`
    margin-left: -1em;
`

function InputField () {
    return (
      <div style={{whiteSpace: 'nowrap'}}>
        <span style={{display: 'inline-block'}}>
        <Input type="text" placeholder='Session name' />
        <Icon src={editIcon} />
          </span>
      </div>
    )
}

export default styled(InputField)`
    
`