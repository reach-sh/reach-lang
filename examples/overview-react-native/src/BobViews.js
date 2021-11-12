import React from 'react';
import { TouchableOpacity, Text, TextInput, View } from 'react-native';
import styles from './Styles';

export class RunBackend extends React.Component {
  render() {
    const { parent } = this.props;
    const { ctcInfoStr } = this.state || {};
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>Alice will deploy the contract.</Text>
        <Text style={styles.textNormal}>Ask Alice for her contract info and paste it here:</Text>
        <TextInput
          onChangeText={e => this.setState({ ctcInfoStr: e })}
          placeholder="{}"
          spellCheck={false}
          style={styles.textInput}
        />
        <TouchableOpacity
          disabled={!ctcInfoStr}
          onPress={() => parent.runBackend(ctcInfoStr)}>
          <Text style={styles.textNormal}>Connect</Text>
        </TouchableOpacity>
      </View>
    );
  }
}

export class ApproveRequest extends React.Component {
  render() {
    const { requestStandard } = this.props;
    if (!requestStandard) {
      return (
        <View style={styles.container}>
          <Text style={styles.textNormal}>
            Once Alice has submitted her requested amount,
            you will be prompted to pay it.
          </Text>
        </View>
      );
    } else {
      return (
        <View style={styles.container}>
          <Text style={styles.textNormal}>
            You have received a prompt to pay Alice's requested amount.
          </Text>
        </View>
      );
    }
  }
}

export class DisplayInfo extends React.Component {
  render() {
    const { info } = this.props;
    if (!info) {
      return (
        <View style={styles.container}>
          <Text style={styles.textNormal}>
            Waiting for Alice to reveal her secret info...
          </Text>
        </View>
      );
    } else {
      return (
        <View style={styles.container}>
          <Text style={styles.textNormal}>
            Alice's secret info is: {info}
          </Text>
          <Text style={styles.textNormal}>
            Thank you, Bob. The contract has run to completion.
          </Text>
        </View>
      );
    }
  }
}

export class BobWrapper extends React.Component {
  render() {
    const { bob } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          {bob}
        </Text>
      </View>
    );
  }
}
