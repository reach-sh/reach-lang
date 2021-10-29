import React from 'react';
import {Button, Text, TextInput, View} from 'react-native';
import styles from './Styles';

const exports = {};

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <View style={styles.App}>
        <View style={styles.AppHeader}>
          <Text style={styles.boldText}>Rock, Paper, Scissors</Text>
          {content}
        </View>
      </View>
    );
  }
};

exports.ConnectAccount = class extends React.Component {
  render() {
    return (
      <Text  style={styles.textNormal}>
        Please wait while we connect to your account. If this takes more than a
        few seconds, there may be something wrong.
      </Text>
    );
  }
};

exports.FundAccount = class extends React.Component {
  render() {
    const {bal, standardUnit, defaultFundAmt, parent} = this.props;
    const amt = (this.state || {}).amt || defaultFundAmt;
    return (
      <View style={styles.container}>
        <Text style={styles.boldTextH2}>Fund account</Text>
        <Text  style={styles.textNormal}>
          Balance: {bal} {standardUnit}
        </Text>
        <Text  style={styles.textNormal}>
          Would you like to fund your account with additional {standardUnit}?
        </Text>
        <Text  style={styles.textNormal}>(This only works on certain devnets)</Text>
        <View style={styles.horizontalContainer}>
          <TextInput
            keyboardType="number-pad"
            placeholder={defaultFundAmt}
            onChangeText={e => this.setState({amt: e})}
          />
          <Button
            onPress={() => parent.fundAccount(amt)}
            title="Fund Account"
          />
          <Button onPress={() => parent.skipFundAccount()} title="Skip" />
        </View>
      </View>
    );
  }
};

exports.DeployerOrAttacher = class extends React.Component {
  render() {
    const {parent} = this.props;
    return (
      <View style={styles.container}>
        <Text style={styles.boldText}>Please select a role:</Text>
        <Button onPress={() => parent.selectDeployer()} title="Deployer" />
        <Text style={styles.boldTextH2}>
          Set the wager, deploy the contract.
        </Text>
        <Button onPress={() => parent.selectAttacher()} title="Attacher" />
        <Text style={styles.boldTextH2}>
          Attach to the Deployer's contract.
        </Text>
      </View>
    );
  }
};

export default exports;
