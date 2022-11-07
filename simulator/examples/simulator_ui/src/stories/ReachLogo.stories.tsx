import { ComponentStory, ComponentMeta } from '@storybook/react';

import ReachLogo from '../componenents/atoms/ReachLogo';

export default {
  title: 'Simulator/Atoms/ReachLogo',
  component: ReachLogo,
  argTypes: {
    backgroundColor: { control: 'color' },
  },
} as ComponentMeta<typeof ReachLogo>;

const Template: ComponentStory<typeof ReachLogo> = (args) => <ReachLogo/>;

export const Primary = Template.bind({});
Primary.parameters = {
    design: {
        type: 'figma',
        url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=314%3A3996'
    }
}
Primary.args = {
};