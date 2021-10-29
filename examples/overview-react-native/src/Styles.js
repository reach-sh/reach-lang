import {StyleSheet} from 'react-native';

const styles = StyleSheet.create({
  boldText: {
    fontWeight: 'bold',
    fontSize: 24,
    color: '#FFFFFF',
  },
  textNormal: {
    color: '#FFFFFF',
  },
  boldTextH2: {
    fontWeight: 'bold',
    fontSize: 18,
    color: '#FFFFFF',
  },
  horizontalContainer: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  container: {
    alignItems: 'center',
  },
  App: {
    textAlign: 'center',
    height: '100%',
  },
  AppHeader: {
    backgroundColor: '#282c34',
    flex: 1,
    display: 'flex',
    alignItems: 'center',
    justifyContent: 'center',
  },
  Deployer: {},
  Attacher: {},
});

export default styles;
