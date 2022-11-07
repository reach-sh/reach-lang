import { ComponentStory, ComponentMeta } from '@storybook/react';

import Navbar from '../componenents/molecules/Navbar';

export default {
  title: 'Simulator/Molecules/Navbar',
  component: Navbar,
  parameters: {
    // More on Story layout: https://storybook.js.org/docs/react/configure/story-layout
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof Navbar>;

const Template: ComponentStory<typeof Navbar> = (args) => <Navbar {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=314%3A4030'
  }
}
Demo.args = {

};
