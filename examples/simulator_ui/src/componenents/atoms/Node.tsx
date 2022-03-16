import styled from 'styled-components'


enum NodeState {
    LOCAL = 'local',
    CONSENSUS = 'consensus',
    SUGGESTED = 'suggested'

}

const NodeContainer = styled.div`
  width: 50%;
`

const Circle = styled.div`
    display: flex;
    justify-content:center;
    align-items:center;
    font-size:32px;
    min-width: 24px;
    max-width: 120px;
    border-radius: 50%;
    aspect-ratio: 1/1;
    text-align: center;
    :hover{
        border: 3px solid var(--white);
    }
`

const LocalStepNode = styled(Circle)`
    background-color: var(--reach-2);
`
const ConsensusStepNode = styled(Circle)`
    background-color: var(--step-1);
        p{
        color: var(--white);
    }
`
const SuggestedStepNode = styled(Circle)`
    background-color: transparent;
    border: 1px dashed var(--white);
    :hover{
        border: 1px dashed var(--white);
    }
`

const NodeLabel = styled.p`
    justify-self: center;
`

function Node ({state}: {state: NodeState}) {
    const isSuggestedStep = state === NodeState.SUGGESTED;
    if(isSuggestedStep){
        return (
            <NodeContainer>
                <SuggestedStepNode/>
            </NodeContainer>
        )
    }
    if(state === NodeState.LOCAL){
        return (
        <NodeContainer> 
            <LocalStepNode>
                <NodeLabel>1</NodeLabel>
            </LocalStepNode>
        </NodeContainer>
    )
    }
    if(state === NodeState.CONSENSUS){
        return (
    <NodeContainer>
        <ConsensusStepNode> 
            <NodeLabel>1</NodeLabel>
        </ConsensusStepNode>
    </NodeContainer>
    )
    }
    else return <div>Total and utter failure</div>;
}

export default styled(Node)`
    
`