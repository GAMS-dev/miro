const path = require('path');
const ESLintPlugin = require('eslint-webpack-plugin');

module.exports = (env, argv) => {
  const isProduction = argv.mode === 'production';

  return {
    mode: isProduction ? 'production' : 'development',
    entry: {
      miro: ['./srcjs/index.js'],
    },
    devtool: isProduction ? 'source-map' : 'eval-source-map',
    output: {
      filename: 'model_update.js',
      path: path.resolve(__dirname, 'www'),
    },
    externals: {
      jquery: 'jQuery',
    },
    optimization: {
      minimize: isProduction,
    },
    plugins: [
      new ESLintPlugin({ fix: true }),
    ],
  };
};
