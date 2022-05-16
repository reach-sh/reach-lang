
import { ComponentStory, ComponentMeta } from '@storybook/react';
import NewSessionIcon from '../componenents/atoms/NewSessionIcon';




// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: 'Simulator/Atoms/NewSessionIcon',
  component: NewSessionIcon,
  argTypes: {
    backgroundColor: { control: 'color' },
  },
} as ComponentMeta<typeof NewSessionIcon>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof NewSessionIcon> = (args) => <NewSessionIcon {...args} />;

export const Primary = Template.bind({});
Primary.parameters = {
  design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=113%3A407'
  }
}