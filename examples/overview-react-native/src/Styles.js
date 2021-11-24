import {StyleSheet, Dimensions} from 'react-native';

const styles = StyleSheet.create({
  boldText: {
    fontWeight: 'bold',
    fontSize: 24,
    color: 'white',
    marginVertical: 10,
  },
  textNormal: {
    color: 'white',
    marginVertical: 10,
    textAlign: 'center',
  },
  boldTextH2: {
    fontWeight: 'bold',
    fontSize: 18,
    color: 'white',
    marginVertical: 10,
  },
  horizontalContainer: {
    flexDirection: 'row',
    alignItems: 'center',
  },
  container: {
    alignItems: 'center',
    width: Dimensions.get('window').width,
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
    minWidth: 150,
    color: 'black'
  },
  Deployer: {},
  Attacher: {},
});

export default styles;
