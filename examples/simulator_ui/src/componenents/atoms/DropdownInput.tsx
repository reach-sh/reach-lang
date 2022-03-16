import styled from 'styled-components'
import { useState } from 'react'

const Input = styled.select`
    width: 156px;
    height: 28px;
    border: 1px solid #3E3E3E;
    box-sizing: border-box;
    filter: drop-shadow(0px 4px 4px rgba(0, 0, 0, 0.25));
    border-radius: 4px;
    background-color: transparent;
    color: var(--off-white);
    font-size: 12px;
    line-height: 100%;
`
const Icon = styled.img`
    margin-left: -1em;
`

function DropdownInput ({value, onChange}: {value: string, onChange: Function}) {
    return (
      <div style={{whiteSpace: 'nowrap'}}>
        <span style={{display: 'inline-block'}}>
        <Input placeholder='Session name' value={value} onChange={(e) => {console.log('target value @dropdownInputs onChange ' + e.target.value); return onChange(e.target.value)}}>
            <option value='0'>Alice</option>
            <option value='1'>Bob</option>
            </Input>
          </span>
      </div>
    )
}

export default styled(DropdownInput)`
    
`