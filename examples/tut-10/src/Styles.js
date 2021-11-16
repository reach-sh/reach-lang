import {StyleSheet} from 'react-native';

const styles = StyleSheet.create({
  boldText: {
    fontWeight: 'bold',
    fontSize: 24,
    color: '#FFFFFF',
    marginVertical: 10,
  },
  textNormal: {
    color: '#FFFFFF',
    marginVertical: 10,
  },
  boldTextH2: {
    fontWeight: 'bold',
    fontSize: 18,
    color: '#FFFFFF',
    marginVertical: 10,
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
  textInput: {
    borderWidth: 1,
    borderColor: 'white',
    backgroundColor: 'white',
    height: 40,
    marginVertical: 10,
  },
  Deployer: {},
  Attacher: {},
});

export default styles;
