import { ComponentStory, ComponentMeta } from '@storybook/react';

import LeftPanel from '../componenents/organisms/LeftPanel';

export default {
  title: 'Simulator/Organisms/LeftPanel',
  component: LeftPanel,
  parameters: {
    // More on Story layout: https://storybook.js.org/docs/react/configure/story-layout
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof LeftPanel>;

const Template: ComponentStory<typeof LeftPanel> = (args) => <LeftPanel {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=91%3A229'
  }
}
Demo.args = {

};
