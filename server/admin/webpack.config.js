const path = require('path');
const TerserJSPlugin = require('terser-webpack-plugin');

module.exports = (env, argv) => ({
    mode: 'development',
    entry: {
      miro: [ './srcjs/index.js' ]
    },
    devtool: 'source-map',
    output: {
      filename: 'model_update.js',
      path: path.resolve(__dirname, "www"),
    },
    externals: {
      jquery: 'jQuery'
    },
    module: {
        rules: [
          {
            enforce: "pre",
            test: /\.js$/,
            exclude: /node_modules/,
            loader: "eslint-loader",
            options: {
              fix: true
            }
          },
          {
            test: /\.js$/,
            exclude: /node_modules/,
            use: {
              loader: 'babel-loader',
              options: {
                "plugins": [
                  ["@babel/plugin-proposal-class-properties", { "loose": true },],
                  ["@babel/plugin-proposal-private-property-in-object", { "loose": true },],
                  ["@babel/plugin-proposal-private-methods", { "loose": true }]
                ],
                presets: [[
                    '@babel/preset-env',
                    {
                        "useBuiltIns": "entry",
                        "corejs": "3"
                    }
                ]]
              }
            }
          }
        ]
    }
});
