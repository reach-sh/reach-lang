import { ComponentStory, ComponentMeta } from '@storybook/react';

import InputField from '../componenents/atoms/InputField';

export default {
  title: 'Simulator/Atoms/InputField',
  component: InputField,
} as ComponentMeta<typeof InputField>;

const Template: ComponentStory<typeof InputField> = (args) => <InputField {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=314%3A4012'
  }
}
Demo.args = {

};
