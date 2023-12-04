const path = require('path');
const TerserPlugin = require('terser-webpack-plugin');
const ESLintPlugin = require('eslint-webpack-plugin');

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
    optimization: {
      minimize: true,
      minimizer: [new TerserPlugin()],
    },
    plugins: [new ESLintPlugin({
      fix: true
    })],
    module: {
        rules: [
          {
            test: /\.js$/,
            exclude: /node_modules/,
            use: {
              loader: 'babel-loader',
              options: {
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
