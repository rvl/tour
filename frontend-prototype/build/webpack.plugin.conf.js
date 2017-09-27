const webpack = require('webpack');
const path = require('path');
var utils = require('./utils')

function resolve (dir) {
  return path.join(__dirname, '..', dir)
}

module.exports = {
  entry: resolve('/src/plugin.js'),
  output: {
    path: resolve('/plugin/'),
    filename: 'tour.min.js',
    libraryTarget: 'window',
    library: 'Tour'
  },
  resolve: {
    extensions: ['.js', '.vue', '.json'],
    alias: {
      'vue$': 'vue/dist/vue.esm.js',
      '@': resolve('src'),
    }
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        loader: 'babel-loader',
        include: resolve('src'),
        exclude: /node_modules/
      },
      {
        test: /\.vue$/,
        loader: 'vue-loader'
      },
      {
        test: /\.(png|jpe?g|gif|svg)(\?.*)?$/,
        loader: 'url-loader',
        options: {
          limit: 10000,
          name: utils.assetsPath('img/[name].[hash:7].[ext]') // fixme: ?
        }
      },
      {
        test: /\.css$/,
        loader: 'style!less!css'
      }
    ]
  },
  // externals: {
  //   moment: 'moment'
  // },
  plugins: [
    new webpack.optimize.UglifyJsPlugin( {
      minimize : true,
      sourceMap : false,
      mangle: true,
      compress: {
        warnings: false
      }
    })
  ]
};
