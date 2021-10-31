import React from 'react';
import { Button, Text, TextInput, View } from 'react-native';
import styles from './Styles';

export class Wrapper extends React.Component {
  render() {
    const { app } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          {app}
        </Text>
      </View>
    );
  }
}

export class ConnectAccount extends React.Component {
  render() {
    return (
      <View style={styles.container}>
        <Text style={styles.textNormal}>
          Please wait while we connect to your account.
          If this takes more than a few seconds, there may be something wrong.
        </Text>
      </View>
    )
  }
}

export class FundAccount extends React.Component {
  constructor(props) {
    super(props);
    this.state = { amt: props.defaultFundAmtStandard };
  }

  render() {
    const { addr, bal, standardUnit, defaultFundAmtStandard, parent } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.boldText}>Fund account</Text>
        <Text style={styles.textNormal}>Address: {addr}</Text>
        <Text style={styles.textNormal}>Balance: {bal} {standardUnit}</Text>
        <Text style={styles.textNormal}>Would you like to fund your account with additional {standardUnit}?</Text>
        <Text style={styles.textNormal}>(This only works on certain devnets)</Text>
        <TextInput
          keyboardType="number-pad"
          placeholder={defaultFundAmtStandard}
          onChangeText={(e) => this.setState({ amt: e })}
        />
        <Button
          onPress={() => parent.fundAccount(this.state.amt)}
          title="Fund Account" />
        <Button
          onPress={() => parent.skipFundAccount()}
          title="Skip" />
      </View>
    );
  }
}

export class SelectRole extends React.Component {
  render() {
    const { parent } = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.boldText}>Please select a role:</Text>
        <Button
          onPress={() => parent.selectAlice()}
          title="Alice" />
        <Text style={styles.textNormal}>Requests payment from Bob in order to reveal a secret.</Text>
        <Button
          onPress={() => parent.selectBob()}
          title="Bob" />
        <Text style={styles.textNormal}>Pays Alice in order for her to reveal a secret.</Text>
      </View>
    );
  }
}
