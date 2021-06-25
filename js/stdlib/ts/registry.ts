import { Connector } from "./ConnectorMode";

let currentConnector : null | true | Connector = null;

export const unsafeAllowMultipleStdlibs = () => {
  currentConnector = true;
};

export const doStdlibLoad = (connector: Connector) => {
  if (currentConnector == null || currentConnector == true || currentConnector == connector) {
    currentConnector = connector;
  } else {
    throw new Error('Cannot load multiple stdlib connectors without using `unsafeAllowMultipleStdlibs`');
  }
};
