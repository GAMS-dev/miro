const path = require('path');
module.exports = {
    mode: 'development',
    entry: {
      miro: './srcjs/miro.js',
      miro_admin: './srcjs/miro_admin.js'
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
                presets: ['@babel/preset-env']
              }
            }
          }
        ]
    }
};
