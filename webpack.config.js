
var path = require('path');
var webpack = require('webpack');
var merge = require('webpack-merge');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var CopyWebpackPlugin = require('copy-webpack-plugin');

// detemine build env
var TARGET_ENV = process.env.npm_lifecycle_event === 'build' ? 'production' : 'development';

// common webpack config
var commonConfig = {
  output: {
    path: path.resolve(__dirname, 'dist/'),
    publicPath: '/',
    filename: '[hash].js'
  },
  resolve: {
    modules: ['node_modules'],
    extensions: ['.js', '.json', '.elm']
  },
  module: {
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.(ttf|eot|svg)(\?[\s\S]+)?$/,
        use: 'file-loader'
      },
      {
        test: /\.woff2?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
        use: [
          {
            loader: 'url-loader',
            options: {
              // Limiting the size of the woff fonts breaks font-awesome ONLY for the extract text plugin
              // limit:10000
            }
          }
        ]
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
      Popper: ['popper.js', 'default'],
      Tether: "tether",
      "window.Tether": "tether",
      Alert: "exports-loader?Alert!bootstrap/js/dist/alert",
      Button: "exports-loader?Button!bootstrap/js/dist/button",
      Carousel: "exports-loader?Carousel!bootstrap/js/dist/carousel",
      Collapse: "exports-loader?Collapse!bootstrap/js/dist/collapse",
      Dropdown: "exports-loader?Dropdown!bootstrap/js/dist/dropdown",
      Modal: "exports-loader?Modal!bootstrap/js/dist/modal",
      Popover: "exports-loader?Popover!bootstrap/js/dist/popover",
      Scrollspy: "exports-loader?Scrollspy!bootstrap/js/dist/scrollspy",
      Tab: "exports-loader?Tab!bootstrap/js/dist/tab",
      Tooltip: "exports-loader?Tooltip!bootstrap/js/dist/tooltip",
      Util: "exports-loader?Util!bootstrap/js/dist/util",
    })
  ]
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
      historyApiFallback: true
      // inline: true
      // progress: true
    },
    module: {
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            'elm-hot-loader',
            {
              loader: 'elm-webpack-loader',
              options: {
                // cwd: __dirname + '/src/',
                debug: true,
                verbose: true
              }
            }
          ]
        },
        {
          test: /\.(css|scss)$/,
          use: ['style-loader', 'css-loader', 'postcss-loader', 'sass-loader']
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
      rules: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          use: 'elm-webpack-loader'
        },
        {
          test: /\.(css|scss)$/,
          use: ExtractTextPlugin.extract('style-loader', [
            'css-loader', 'postcss-loader', 'sass-loader'
          ])
        }
      ]
    },
    plugins: [
      new CopyWebpackPlugin([
        {
          from: 'static/favicon.ico'
        },
        {
          from: 'static/img/',
          to: 'img/'
        },
        {
          from: 'static/manifest.json'
        }
      ]),
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
