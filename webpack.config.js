const path = require('path');
module.exports = {
    mode: 'development',
    entry: {
      miro: './srcjs/miro.js',
      miro_admin: './srcjs/miro_admin.js'
    },
    devtool: 'source-map',
    output: {
      filename: '[name].js',
      path: path.resolve(__dirname, "/www"),
    },
    module: {
        rules: [
          {
            test: /\.js$/,
            exclude: /node_modules/,
            use: {
              loader: 'babel-loader',
              options: {
                presets: ['@babel/preset-env']
              }
            }
          }
        ]
    }
};
