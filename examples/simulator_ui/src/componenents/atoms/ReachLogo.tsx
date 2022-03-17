import styled from 'styled-components'
import reach from '../../assets/reach_logo.svg'

export default function ReachLogo () {
    return (<StyledLogo src={reach}/>)
}

 const StyledLogo = styled.img`
    height: 28px;
    width: 106px;
`