import styled from 'styled-components'
import ReachLogo from '../atoms/ReachLogo'
import InputField from '../atoms/InputField'
import NewSessionButton from './NewSessionButton'
import SessionsDropdown from './SessionsDropdown'



const NavbarContainer = styled.div`
    display: grid;
    background-color: #101010;
    grid-template-columns: 20% 60% 20%;
    padding: 25px;
    width: 100vw;
    border: 1px solid #F2F2F2;
`

const Left = styled.div`
    display: flex;
    flex-direction: row;
`

const Right = styled.div`
    display: flex;
    flex-direction: row;
    grid-column-start: 3;
`

function Navbar () {
    return (
        <NavbarContainer>
            <Left>
                <ReachLogo/>
                <InputField />
            </Left>
            <Right>
                <NewSessionButton />
                <SessionsDropdown/>
            </Right>
        </NavbarContainer>
    )
}

export default styled(Navbar)`

`