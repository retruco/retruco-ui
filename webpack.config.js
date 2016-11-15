
var path = require('path');
var webpack = require('webpack');
var merge = require('webpack-merge');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var autoprefixer = require('autoprefixer');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');

// detemine build env
var TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';

// common webpack config
var commonConfig = {
  output: {
    path: path.resolve(__dirname, 'dist/'),
    publicPath: '/',
    filename: '[hash].js',
  },
  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm']
  },
  module: {
    noParse: /\.elm$/,
    loaders: [
      {
        test: /\.(ttf|eot|svg)(\?[\s\S]+)?$/,
        loader: 'file'
      },
      {
        test: /\.woff2?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        // Limiting the size of the woff fonts breaks font-awesome ONLY for the extract text plugin
        // loader: 'url?limit=10000'
        loader: 'url'
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: 'static/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new webpack.ProvidePlugin({
      $: "jquery",
      jQuery: "jquery",
      "window.jQuery": "jquery",
      Tether: "tether",
      "window.Tether": "tether",
      Tooltip: "exports?Tooltip!bootstrap/js/dist/tooltip",
      Alert: "exports?Alert!bootstrap/js/dist/alert",
      Button: "exports?Button!bootstrap/js/dist/button",
      Carousel: "exports?Carousel!bootstrap/js/dist/carousel",
      Collapse: "exports?Collapse!bootstrap/js/dist/collapse",
      Dropdown: "exports?Dropdown!bootstrap/js/dist/dropdown",
      Modal: "exports?Modal!bootstrap/js/dist/modal",
      Popover: "exports?Popover!bootstrap/js/dist/popover",
      Scrollspy: "exports?Scrollspy!bootstrap/js/dist/scrollspy",
      Tab: "exports?Tab!bootstrap/js/dist/tab",
      Tooltip: "exports?Tooltip!bootstrap/js/dist/tooltip",
      Util: "exports?Util!bootstrap/js/dist/util"
    })
  ],
  postcss: [ autoprefixer( { browsers: ['last 2 versions'] } ) ],
}

// additional webpack settings for local env (when invoked by 'npm start')
if (TARGET_ENV === 'development') {
  console.log( 'Serving locally...');
  module.exports = merge(commonConfig, {
    entry: [
      'webpack-dev-server/client?http://localhost:3001',
      'font-awesome-loader!./static/font-awesome/font-awesome.config.js',
      'bootstrap-loader',
      path.join(__dirname, 'static/index.js')
    ],
    devServer: {
      historyApiFallback: true,
      inline: true,
      progress: true
    },
    module: {
      loaders: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-hot!elm-webpack?verbose=true&warn=true'
        },
        {
          test: /\.(css|scss)$/, 
          loaders: ['style', 'css', 'postcss', 'sass']
        }
      ]
    }
  });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (TARGET_ENV === 'production') {
  console.log('Building for prod...');
  module.exports = merge(commonConfig, {
    entry: [
      'font-awesome-loader',
      'font-awesome-loader!./static/font-awesome/font-awesome.config.js',
      'bootstrap-loader/extractStyles',
      path.join(__dirname, 'static/index.js')
    ],
    module: {
      loaders: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-webpack'
        },
        {
          test: /\.(css|scss)$/,
          loader: ExtractTextPlugin.extract('style-loader', [
            'css', 'postcss', 'sass'
          ])
        }
      ]
    },
    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'static/img/',
          to: 'img/'
        },
        {
          from: 'static/favicon.ico'
        },
      ]),
      new webpack.optimize.OccurenceOrderPlugin(),
      // extract CSS into a separate file
      new ExtractTextPlugin( './[hash].css', { allChunks: true } ),
      // minify & mangle JS/CSS
      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        compressor: { warnings: false }
        // mangle:  true
      })
    ]
  });
}
