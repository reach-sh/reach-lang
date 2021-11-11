import React from 'react';
import { Button, Text, TextInput, View } from 'react-native';
import styles from './Styles';

const sleep = (milliseconds) => new Promise(resolve => setTimeout(resolve, milliseconds));

export class Deploy extends React.Component {
  render() {
    const { parent } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          As Alice, it is your job to deploy the contract.
        </Text>
        <Button title="Deploy" onPress={() => parent.deploy()} />
      </View>
    );
  }
}

export class EnterInfo extends React.Component {
  render() {
    const { parent, defaultInfo } = this.props;
    const { info } = this.state || {};
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>Alice, what is your secret info?</Text>
        <TextInput
          onChangeText={(e) => this.setState({ info: e })}
          placeholder={defaultInfo}
          style={styles.textInput}
        />
        <Button
          onPress={() => parent.enterInfo(info || defaultInfo)}
          title="Submit secret info" />
      </View>
    );
  }
}

export class EnterRequest extends React.Component {
  render() {
    const { parent, standardUnit, defaultRequestStandard } = this.props;
    const { req } = this.state || {};
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          Alice, how much {standardUnit} should Bob pay you
          to reveal this info?
        </Text>
        <TextInput
          keyboardType="number-pad"
          onChangeText={(e) => this.setState({ req: e.currentTarget.value })}
          placeholder={defaultRequestStandard}
          style={styles.textInput}
        />
        <Button onPress={() => parent.enterRequest(req || defaultRequestStandard)} title="Submit request" />
      </View>
    );
  }
}

export class RunBackend extends React.Component {
  render() {
    const { parent, info, requestStandard, standardUnit } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          You request {requestStandard} {standardUnit + ' '}
          to reveal secret info: {info}
        </Text>
        <Text style={styles.boldText}>
          Ready to connect to the contract?
        </Text>
        <Text style={styles.textNormal}>
          You will be prompted to pay for two transactions.
          The first transaction will publish your requested amount,
          and the second will publish your secret while simultaneously
          retrieving the requested amount from the contract.
        </Text>
        <Button
          title="Connect"
          onPress={() => parent.runBackend()}
        />
      </View>
    );
  }
}

export class BackendRunning extends React.Component {
  constructor(props) {
    super(props);
    this.buttonRef = React.createRef();
  }

  async copyToClipborad(button) {
    const { ctcInfoStr } = this.props;
    Clipboard.setString(ctcInfoStr);
    const origInnerHTML = button.title;
    button.title = 'Copied!';
    button.disabled = true;
    await sleep(1000);
    button.title = origInnerHTML;
    button.disabled = false;
  }

  render() {
    const { ctcInfoStr } = this.props;
    if (ctcInfoStr === undefined) {
      return (
        <Text style={styles.textNormal}>
          Waiting for the contract to deploy...
          If this takes more than 1 min, something may be wrong.</Text>
      )
    } else {
      return (
        <View style={styles.container}>
          <Text style={styles.textNormal}>Contract Info</Text>
          <Text style={styles.boldTextH2}>
            The contract is running!
            Please give Bob the following contract info.
          </Text>
          <Text style={styles.textNormal}>{ctcInfoStr}</Text>
          <Button
            ref={this.buttonRef}
            title="Copy to clipboard"
            onPress={e => this.copyToClipborad(this.buttonRef.current)}
          />
          <Text style={styles.textNormal}>You will be automatically prompted to approve the next transaction
            once Bob has paid the requested amount into the contract.</Text>
        </View>
      );
    }
  }
}

export class BackendRan extends React.Component {
  render() {
    return (
      <Text style={styles.textNormal}>
        Thank you, Alice.
        The contract has run to completion.</Text>
    );
  }
}

export class AliceWrapper extends React.Component {
  render() {
    const { alice } = this.props;
    return (
      <Text style={styles.textNormal}>
        {alice}
      </Text>
    );
  }
}