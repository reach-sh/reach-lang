import { ComponentStory, ComponentMeta } from '@storybook/react';

import Node from '../componenents/atoms/Node';

export default {
  title: 'Simulator/Atoms/Node',
  component: Node,
} as ComponentMeta<typeof Node>;

const Template: ComponentStory<typeof Node> = (args) => <Node {...args} />;

export const LocalStep = Template.bind({});
LocalStep.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=131%3A2648'
  }
}
LocalStep.args = {
  state: 'local'
};

export const ConsensusStep = Template.bind({})
ConsensusStep.parameters = {
  design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=131%3A2652'
  }
}
ConsensusStep.args = {
  state: 'consensus'
};


export const SuggestedStep = Template.bind({})
SuggestedStep.parameters = {
  design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=353%3A4978'
  }
}
SuggestedStep.args = {
  state: 'suggested'
};
