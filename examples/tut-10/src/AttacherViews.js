import React from 'react';
import {Text, TextInput, TouchableOpacity, View} from 'react-native';
import PlayerViews from './PlayerViews';
import styles from './Styles';

const exports = {...PlayerViews};

exports.Wrapper = class extends React.Component {
  render() {
    const {content} = this.props;
    return (
      <View style={styles.Attacher}>
        <Text style={styles.boldTextH2}>Attacher (Bob)</Text>
        {content}
      </View>
    );
  }
};

exports.Attach = class extends React.Component {
  render() {
    const {parent} = this.props;
    const {ctcInfoStr} = this.state || {};
    return (
      <View>
        <Text  style={styles.textNormal}>Please paste the contract info to attach to:</Text>
        <TextInput
          onChangeText={e => this.setState({ctcInfoStr: e})}
          placeholder="{}"
          spellCheck={false}
          style={styles.textInput}
        />
        <TouchableOpacity
          disabled={!ctcInfoStr}
          onPress={() => parent.attach(ctcInfoStr)}>
          <Text  style={styles.textNormal}>Attach</Text>
        </TouchableOpacity>
      </View>
    );
  }
};

exports.Attaching = class extends React.Component {
  render() {
    return <Text  style={styles.textNormal}>Attaching, please wait...</Text>;
  }
};

exports.AcceptTerms = class extends React.Component {
  render() {
    const {wager, standardUnit, parent} = this.props;
    const {disabled} = this.state || {};
    return (
      <View>
        <Text style={styles.boldTextH2}>The terms of the game are:</Text>
        <Text  style={styles.textNormal}>
          Wager: {wager} {standardUnit}
        </Text>
        <TouchableOpacity
          disabled={disabled}
          onPress={() => {
            this.setState({disabled: true});
            parent.termsAccepted();
          }}>
          <Text  style={styles.textNormal}>Accept terms and pay wager</Text>
        </TouchableOpacity>
      </View>
    );
  }
};

exports.WaitingForTurn = class extends React.Component {
  render() {
    return (
      <View>
        <Text  style={styles.textNormal}>Waiting for the other player...</Text>
        <Text style={styles.boldTextH2}>
          Think about which move you want to play.
        </Text>
      </View>
    );
  }
};

export default exports;
