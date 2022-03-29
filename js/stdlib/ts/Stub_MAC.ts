// Notice how webpack.config.js replaces this
export default class MyAlgoConnect {
  constructor(...args: any) {
    void(args);
    throw Error(`Constructing MyAlgoConnect with the non-webpacked version of @reach-sh/stdlib is not supported.`);
  }
};
