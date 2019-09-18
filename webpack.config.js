const path = require('path');
const TerserJSPlugin = require('terser-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const OptimizeCSSAssetsPlugin = require('optimize-css-assets-webpack-plugin');
const RemovePlugin = require('remove-files-webpack-plugin');
module.exports = {
    mode: 'development',
    entry: {
      skin_browser: './less/skins/browser.js',
      skin_light: './less/skins/light.js',
      skin_dark: './less/skins/dark.js',
      miro: './srcjs/miro.js',
      miro_admin: './srcjs/miro_admin.js'
    },
    optimization: {
      minimizer: [new TerserJSPlugin({sourceMap: true,}), new OptimizeCSSAssetsPlugin({})],
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
      new RemovePlugin({
          after: {
            test: [{
              folder: path.resolve(__dirname, "www"),
              method: (filePath) => {
                  return new RegExp(/skin_.+\.js(\.map)?$/, 'm').test(filePath);
              }
            }]
          }
      })
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
              {
                loader: MiniCssExtractPlugin.loader,
              },
              {
                loader: 'css-loader',
              },
              {
                loader: 'less-loader',
                options: {
                  sourceMap: true,
                },
              },
            ]
          },
          {
            test: /\.(png|jp(e*)g|svg)$/,  
            use: [{
                loader: 'url-loader',
                options: { 
                    limit: 8000, // Convert images < 8kb to base64 strings
                    name: 'images/[hash]-[name].[ext]'
                } 
            }]
          }
        ]
    }
};
