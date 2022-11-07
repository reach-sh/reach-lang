import { ComponentStory, ComponentMeta } from '@storybook/react';

import Visualizer from '../componenents/organisms/Visualizer';

export default {
  title: 'Simulator/Organisms/Visualizer',
  component: Visualizer,
  parameters: {
    // More on Story layout: https://storybook.js.org/docs/react/configure/story-layout
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof Visualizer>;

const Template: ComponentStory<typeof Visualizer> = (args) => <Visualizer {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=91%3A176'
  }
}
Demo.args = {
  
};
