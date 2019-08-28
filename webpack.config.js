const path = require('path');
const TerserJSPlugin = require('terser-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
module.exports = {
    mode: 'development',
    entry: {
      miro: './srcjs/miro.js',
      miro_admin: './srcjs/miro_admin.js'
    },
    optimization: {
      minimizer: [new TerserJSPlugin({}), new OptimizeCSSAssetsPlugin({})],
    },
    devtool: 'source-map',
    output: {
      library: 'Miro',
      filename: '[name].js',
      path: path.resolve(__dirname, "www"),
    },
    externals: {
      jquery: 'jQuery'
    },
    plugins: [
      new MiniCssExtractPlugin({
        // Options similar to the same options in webpackOptions.output
        // all options are optional
        filename: '[name].css',
        chunkFilename: '[id].css',
        ignoreOrder: false, // Enable to remove warnings about conflicting order
      }),
    ],
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
                  ["@babel/plugin-proposal-class-properties", { "loose": true }]
                ],
                presets: [[
                    '@babel/preset-env',
                    {
                        "useBuiltIns": "entry"
                    }
                ]]
              }
            }
          },
          {
            test: /\.less$/,
            exclude: /node_modules/,
            use: [
                MiniCssExtractPlugin.loader,
                "css-loader",
                "less-loader"
            ]
          }
        ]
    }
};
