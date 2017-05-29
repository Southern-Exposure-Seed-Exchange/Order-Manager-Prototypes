var path = require("path");
var CleanWebpackPlugin = require('clean-webpack-plugin');
var bourbon = require('node-bourbon').includePaths;
var neat = require('node-neat').includePaths;

module.exports = {
  entry: {
    app: [
      './src/index.js',
    ],
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js'
  },

  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        use: {
          loader: 'file-loader',
          options: {
            name: '[name].[ext]',
          },
        },
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/,],
        use: [
          { loader: 'elm-hot-loader' },
          { loader: 'elm-webpack-loader',
            options: {
              warn: true
            },
          },
        ],
      },
      {
        test: /\.sass$/,
        use: [
          { loader: 'style-loader' },
          { loader: 'css-loader' },
          { loader: 'sass-loader',
            options: {
              includePaths: bourbon.concat(neat)
            }
          },
        ],
      },
    ],

    noParse: /\.elm$/,
  },

  devServer: {
    inline: true,
    host: "0.0.0.0",
    disableHostCheck: true,
    stats: {
      colors: true,
      chunks: false,
    },
    proxy: {
      '/api/*': {
        target: 'http://localhost:3000',
        changeOrigin: true,
        pathRewrite: { "^/api/": "" },
      }
    }
  },


  plugins: [
    new CleanWebpackPlugin('elm-stuff/build-artifacts/0.17.1/user'),
  ]

};
