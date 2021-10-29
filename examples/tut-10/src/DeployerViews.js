import React from 'react';
import {Button, Text, TextInput, View} from 'react-native';
import PlayerViews from './PlayerViews';
import styles from './Styles';
import Clipboard from '@react-native-clipboard/clipboard';
const exports = {...PlayerViews};

const sleep = milliseconds =>
  new Promise(resolve => setTimeout(resolve, milliseconds));

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <View style={styles.Deployer}>
        <Text style={styles.boldTextH2}>Deployer (Alice)</Text>
        {content}
      </View>
    );
  }
};

exports.SetWager = class extends React.Component {
  render() {
    const {parent, defaultWager, standardUnit} = this.props;
    const wager = (this.state || {}).wager || defaultWager;
    return (
      <View style={styles.container}>
        <View style={styles.horizontalContainer}>
          <TextInput
            keyboardType="number-pad"
            placeholder={defaultWager}
            onChangeText={e => this.setState({wager: e})}
          />
          <Text  style={styles.textNormal}>{standardUnit}</Text>
        </View>
        <Button title="Set wager" onPress={() => parent.setWager(wager)} />
      </View>
    );
  }
};

exports.Deploy = class extends React.Component {
  render() {
    const {parent, wager, standardUnit} = this.props;
    return (
      <View style={styles.container}>
        <Text  style={styles.textNormal}>
          Wager (pay to deploy): <Text style={styles.boldText}>{wager}</Text>{' '}
          {standardUnit}
        </Text>
        <Button title="Deploy" onPress={() => parent.deploy()} />
      </View>
    );
  }
};

exports.Deploying = class extends React.Component {
  render() {
    return <Text  style={styles.textNormal}>Deploying... please wait.</Text>;
  }
};

exports.WaitingForAttacher = class extends React.Component {
  constructor(props) {
    super(props);
    this.buttonRef = React.createRef();
  }

  async copyToClipborad(button) {
    const {ctcInfoStr} = this.props;
    Clipboard.setString(ctcInfoStr);
    const origInnerHTML = button.title;
    button.title = 'Copied!';
    button.disabled = true;
    await sleep(1000);
    button.title = origInnerHTML;
    button.disabled = false;
  }

  render() {
    const {ctcInfoStr} = this.props;
    return (
      <View style={styles.container}>
        <Text  style={styles.textNormal}>Waiting for Attacher to join...</Text>
        <Text style={styles.boldTextH2}>
          Please give them this contract info:
        </Text>
        <Text  style={styles.textNormal}>{ctcInfoStr}</Text>
        <Button
          ref={this.buttonRef}
          title="Copy to clipboard"
          onPress={e => this.copyToClipborad(this.buttonRef.current)}
        />
      </View>
    );
  }
};

export default exports;
