import { ComponentStory, ComponentMeta } from '@storybook/react';
import Button from '../componenents/atoms/Button';
import NewSessionIcon from '../componenents/atoms/NewSessionIcon';
import ExpandIcon from '../componenents/atoms/ExpandIcon'
// More on default export: https://storybook.js.org/docs/react/writing-stories/introduction#default-export
export default {
  title: 'Simulator/Atoms/Button',
  component: Button,
  argTypes: {
    backgroundColor: { control: 'color' },
  },
} as ComponentMeta<typeof Button>;

// More on component templates: https://storybook.js.org/docs/react/writing-stories/introduction#using-args
const Template: ComponentStory<typeof Button> = (args) => <Button {...args} />;

export const InitParticipant = Template.bind({});
// More on args: https://storybook.js.org/docs/react/writing-stories/args
InitParticipant.args = {
  designs: {
    type: 'figma',
    url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=91%3A203'
  },
  primary: true,
  label: 'Init Participants',
};

export const NewSession = Template.bind({});
NewSession.args = {
  label: 'NewSession',
  icon: <NewSessionIcon />,
  backgroundColor: '#FFFFFF',
  color: '#101010'
};

export const ExpandSessionList = Template.bind({});
ExpandSessionList.args = {
  icon: <ExpandIcon />,
  label: 'Sessions',
  backgroundColor: 'transparent',
  color: "#FFFFFFF",
};

export const Small = Template.bind({});
Small.args = {
  size: 'small',
  label: 'Button',
};
