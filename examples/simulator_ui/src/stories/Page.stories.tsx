import { ComponentStory, ComponentMeta } from '@storybook/react';
import App from '../App';

export default {
  title: 'Example/Simulator',
  component: App,
  parameters: {
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof App>;

const Template: ComponentStory<typeof App> = () => <App  />;

export const Simulator = Template.bind({})


Simulator.parameters = {
}
Simulator.args = {}


// More on interaction testing: https://storybook.js.org/docs/react/writing-tests/interaction-testing
// LoggedIn.play = async ({ canvasElement }) => {
//   const canvas = within(canvasElement);
//   const loginButton = await canvas.getByRole('button', { name: /Log in/i });
//   await userEvent.click(loginButton);
// };
