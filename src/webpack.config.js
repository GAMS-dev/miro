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
    entry: argv.mode === 'development' ? {
      default_browser: './less/skins/default/browser.js',
      miro: ['./srcjs/miro.js'],
      miro_admin: ['./srcjs/miro_admin.js']
    } : {
      default_browser: './less/skins/default/browser.js',
      default_light: './less/skins/default/light.js',
      default_dark: './less/skins/default/dark.js',
      '../../server/proxy/templates/2col/assets/css/themes/default': '../server/proxy/less/themes/default/styles.js',
      '../../server/proxy/templates/2col/assets/css/themes/redwine': '../server/proxy/less/themes/redwine/styles.js',
      '../../server/proxy/templates/2col/assets/css/themes/tawny': '../server/proxy/less/themes/tawny/styles.js',
      '../../server/proxy/templates/2col/assets/css/themes/blackandwhite': '../server/proxy/less/themes/blackandwhite/styles.js',
      '../../server/proxy/templates/2col/assets/css/themes/darkblue': '../server/proxy/less/themes/darkblue/styles.js',
      '../../server/proxy/templates/2col/assets/css/themes/forest': '../server/proxy/less/themes/forest/styles.js',
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
    plugins: [
      ...plugins,
      new RemovePlugin({
          after: {
            test: [{
              folder: path.resolve(__dirname, "www"),
              method: (filePath) => {
                  return new RegExp(/default_.+\.js(\.map)?$/, 'm').test(filePath) ||
                    filePath.endsWith('.js.LICENSE.txt');
              }
            },
            {
              folder: path.resolve(__dirname, "..", "server", "proxy", "templates", "2col", "assets", "css", "themes"),
              method: (filePath) => {
                  return new RegExp(/.js(\.map)?$/, 'm').test(filePath);
              }
            }],
            allowRootAndOutside: true
          }
      })
    ],
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
            type: 'asset/inline'
          }
        ]
    }
});
