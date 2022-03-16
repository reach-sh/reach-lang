import {ReactElement} from 'react';
import styled from 'styled-components'
interface ButtonProps {
  /**
   * Is this the principal call to action on the page?
   */
  primary?: boolean;
  /**
   * What background color to use
   */
  backgroundColor?: string;
  /**
   * How large should the button be?
   */
  size?: 'small' | 'medium' | 'large';
  /**
   * Button contents
   */
  label: string;
  icon: ReactElement;
  color?: string;
  labelWeight?: string;
  /**
   * Optional click handler
   */
  onClick?: () => void;
}

/**
 * Primary UI component for user interaction
 */

const Label = styled.p`
width: 78px;
height: 12px;
font-family: "Roboto", sans-serif;
font-style: normal;
font-weight: normal;
font-size: 12px;
line-height: 100%;
color: #F2F2F2;
flex: none;
order: 0;
flex-grow: 0;
margin: 0px 10px;
white-space: nowrap;
`

const ButtonLabel = ({label, color }: {label: string, color: string}) => { 
return (
  <Label style={{color}}>{label} </Label>
)}

const Button = ({
  primary = false,
  size = 'medium',
  backgroundColor,
  label,
  icon,
  color,
  ...props
}: ButtonProps) => {
  const mode = primary ? 'storybook-button--primary' : 'storybook-button--secondary';
  return (
    <button
      type="button"
      style={{ backgroundColor, color }}
      {...props}
    >
      {label ? <ButtonLabel label={label} color={color ? color : '#F2F2F2'}/> : ''}
      {icon? icon : ''}
    </button>
  );
};

export default styled(Button)`
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: center;
  padding: 8px 24px;
  width: 156px;
  /* height: 28px; */
  background: #F45747;
  border-radius: 16px;
  border: none;
`