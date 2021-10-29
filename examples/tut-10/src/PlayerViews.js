import React from 'react';
import {Button, Text, View} from 'react-native';
import styles from './Styles';

const exports = {};

exports.GetHand = class extends React.Component {
  render() {
    const {parent, playable, hand} = this.props;
    return (
      <View style={styles.container}>
        {hand && <Text  style={styles.textNormal}>It was a draw! Pick again.</Text>}
        {!playable && <Text style={styles.boldText}>Please wait...</Text>}
        <Button
          onPress={() => parent.playHand('ROCK')}
          disabled={!playable}
          title="Rock"
        />
        <Button
          onPress={() => parent.playHand('PAPER')}
          disabled={!playable}
          title="Pager"
        />
        <Button
          onPress={() => parent.playHand('v')}
          disabled={!playable}
          title="Scissors"
        />
      </View>
    );
  }
};

exports.WaitingForResults = class extends React.Component {
  render() {
    return <Text  style={styles.textNormal}>Waiting for results...</Text>;
  }
};

exports.Done = class extends React.Component {
  render() {
    const {outcome} = this.props;
    return (
      <Text  style={styles.textNormal}>
        Thank you for playing. The outcome of this game was:
        <Text style={styles.boldText}>{outcome || 'Unknown'}</Text>
      </Text>
    );
  }
};

exports.Timeout = class extends React.Component {
  render() {
    return <Text  style={styles.textNormal}>There's been a timeout. (Someone took too long.)</Text>;
  }
};

export default exports;
