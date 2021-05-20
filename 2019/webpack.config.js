const CopyPlugin = require("copy-webpack-plugin");

module.exports = {
  mode: "development",

  target: "node",
  node: {
    __dirname: false,
    __filename: false,
  },

  devServer: {
    contentBase: ["./dist", "./public"],
  },

  resolve: {
    // Add '.ts' and '.tsx' as resolvable extensions.
    extensions: [".ts", ".tsx", ".js"],
  },

  module: {
    rules: [
      {
        test: /\.ts(x?)$/,
        exclude: /node_modules/,
        use: [
          {
            loader: "ts-loader",
          },
        ],
      },
      // All output '.js' files will have any sourcemaps re-processed by 'source-map-loader'.
      {
        enforce: "pre",
        test: /\.js$/,
        loader: "source-map-loader",
      },
    ],
  },

  plugins: [new CopyPlugin([{ from: "static", to: "" }])],
};
