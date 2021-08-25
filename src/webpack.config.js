const path = require('path');
const TerserJSPlugin = require('terser-webpack-plugin');
const MiniCssExtractPlugin = require('mini-css-extract-plugin');
const CssMinimizerPlugin = require('css-minimizer-webpack-plugin');
const RemovePlugin = require('remove-files-webpack-plugin');
const ESLintPlugin = require('eslint-webpack-plugin');
const plugins = [
   new MiniCssExtractPlugin({
    // Options similar to the same options in webpackOptions.output
    // all options are optional
    filename: '[name].css',
    chunkFilename: '[id].css',
    ignoreOrder: false, // Enable to remove warnings about conflicting order
  }),
  new ESLintPlugin({
    fix: true
  })]
module.exports = (env, argv) => ({
    mode: 'development',
    entry: {
      skin_browser: './less/skins/browser.js',
      skin_light: './less/skins/light.js',
      skin_dark: './less/skins/dark.js',
      miro: ['./srcjs/miro.js'],
      miro_admin: ['./srcjs/miro_admin.js']
    },
    optimization: {
      minimize: true,
      minimizer: [new TerserJSPlugin({}), new CssMinimizerPlugin({})],
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
    plugins: argv.mode === 'production'? [
      ...plugins,
      new RemovePlugin({
          after: {
            test: [{
              folder: path.resolve(__dirname, "www"),
              method: (filePath) => {
                  return new RegExp(/skin_.+\.js(\.map)?$/, 'm').test(filePath) ||
                    filePath.endsWith('.js.LICENSE.txt');
              }
            }]
          }
      })
    ]: [...plugins],
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
                        "useBuiltIns": "entry",
                        "corejs": "3.15"
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
                options: {
                  publicPath: ''
                }
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
});
