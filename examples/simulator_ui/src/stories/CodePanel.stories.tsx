import { ComponentStory, ComponentMeta } from '@storybook/react';

import CodePanel from '../componenents/organisms/CodePanel';
const code =  `
'reach 0.1';

export const main =
   Reach.App(
      {},
      [Participant('Alice', { request: UInt,
            info: Bytes(128) }),
       Participant('Bob', { want: Fun([UInt], Null),
            got: Fun([Bytes(128)], Null) })],
      (A, B) => {
         (A.only(() => {
            const request = declassify(interact.request); });
         A.publish(request);

export const main =
   Reach.App(
      {},
      [Participant('Alice', { request: UInt,
            info: Bytes(128) }),
       Participant('Bob', { want: Fun([UInt], Null),
            got: Fun([Bytes(128)], Null) })],
      (A, B) => {
         (A.only(() => {
            const request = declassify(interact.request); });
         A.publish(request);|'reach 0.1';

export const main =
   Reach.App(
      {},
      [Participant('Alice', { request: UInt,
            info: Bytes(128) }),
       Participant('Bob', { want: Fun([UInt], Null),
            got: Fun([Bytes(128)], Null) })],
      (A, B) => {
         (A.only(() => {
            const request = declassify(interact.request); });
         A.publish(request);|'reach 0.1';

export const main =
   Reach.App(
      {},
      [Participant('Alice', { request: UInt,
            info: Bytes(128) }),
       Participant('Bob', { want: Fun([UInt], Null),
            got: Fun([Bytes(128)], Null) })],
      (A, B) => {
         (A.only(() => {
            const request = declassify(interact.request); });
         A.publish(request);|`;
         
export default {
  title: 'Simulator/Organisms/CodePanel',
  component: CodePanel,
  parameters: {
    layout: 'fullscreen',
  },
} as ComponentMeta<typeof CodePanel>;

const Template: ComponentStory<typeof CodePanel> = (args) => <CodePanel {...args} />;

export const Demo = Template.bind({});
Demo.parameters = {
    design: {
        type: 'figma',
        url: 'https://www.figma.com/file/ye16N2xj0RQDYIARy8zTpy/Reach-Simulator?node-id=338%3A13696'
    }
}
Demo.args = {
  code: code
};
