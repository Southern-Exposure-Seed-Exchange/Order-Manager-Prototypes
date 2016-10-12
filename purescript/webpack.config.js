var path = require("path");

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },
  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js'
  },

  module: {
    loaders: [
      { test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file?name=[name].[ext]'
      },
      { test: /.purs$/,
        exclude: /node_modules/,
        loader: 'purs-loader',
        query: {
          psc: 'psa',
          pscArgs: {
            'stash': null,
            'censor-lib': null,
            'source-maps': null,
          },
          pscIde: true,
          warnings: true,
        }
      }
    ]
  },

  resolve: { modulesDirectories: ['node_modules', 'bower_components'],
             extensions: ['', '.purs', '.js'] },

  devServer: {
    inline: true,
    stats: {
      colors: true,
      chunks: false,
    },
    proxy: {
      '/api/*': {
        target: 'http://localhost:3000',
        changeOrigin: true,
        pathRewrite: { "^/api": "" },
      }
    }
  },

};
