import { ComponentStory, ComponentMeta } from '@storybook/react';
import DropdownInput from '../componenents/atoms/DropdownInput';
import ExpandIcon from '../componenents/atoms/ExpandIcon'
// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: 'Simulator/Atoms/DropdownInput',
  component: DropdownInput,
} as ComponentMeta<typeof DropdownInput>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof DropdownInput> = (args) => <DropdownInput {...args} />;

export const Demo = Template.bind({});
// More on args: https://storybook.js.org/docs/react/writing-stories/args
Demo.args = {
  designs: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=91%3A209'
  },
  primary: true,
};

