import { ComponentStory, ComponentMeta } from '@storybook/react';

import RightPanel from '../componenents/organisms/RightPanel';

export default {
  title: 'Simulator/Organisms/RightPanel',
  component: RightPanel,
  parameters: {
    // More on Story layout: https://storybook.js.org/docs/react/configure/story-layout
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof RightPanel>;

const Template: ComponentStory<typeof RightPanel> = (args) => <RightPanel {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=98%3A146'
  }
}
Demo.args = {

};
