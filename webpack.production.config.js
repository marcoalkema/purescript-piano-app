var path = require('path');
var webpack = require('webpack');
var PurescriptWebpackPlugin = require('purescript-webpack-plugin');
var HtmlWebpackPlugin = require('html-webpack-plugin');

module.exports = {
  entry: [ path.join(__dirname, 'support/index.js') ],
  debug: false,
  output: {
    path: path.join(__dirname, '/dist/'),
    filename: '[name]-[hash].min.js',
    publicPath: '/'
  },
  module: {
    loaders: [
        { test: /\.purs$/, loader: 'purs-loader' },
        { test: /\.js$/,   loader: "webpack-strip?strip[]=console.log" },
        { test: /\.css$/,  loader: 'style!css-loader?modules&importLoaders=1&localIdentName=[name]__[local]___[hash:base64:5]'},
        { test: /\.png$/,  loader: "url-loader?limit=100000" },
        { test: /\.jpg$/,  loader: "file-loader" },
	{ test: /\.png$/, loader: "file-loader" },
        { test: /\.jpg$/,  loader: "file-loader" },
	{  // ASSET LOADER
	    test: /\.(woff|woff2|ttf|eot)$/,
	    loader: 'file'
	},
	{
	    //IMAGE LOADER
	    test: /\.(jpe?g|png|gif|svg)$/i,
	    loader:'file'
	},
{
    // HTML LOADER
    test: /\.html$/,
    loader: 'html-loader'
},
	{
	    //SCSS LOADER
  test: /\.scss$/,
	    loaders: ["style", "css", "sass?indentedSyntax"]
	}
	]
  },
  plugins: [
    new PurescriptWebpackPlugin({
      src: ['bower_components/purescript-*/src/**/*.purs', 'src/**/*.purs'],
      ffi: ['bower_components/purescript-*/src/**/*.js',   'src/**/*.js'],
      bundle: true,
      psc: 'psa'
    }),
    new webpack.DefinePlugin({
      'process.env': {
        'process.env.NODE_ENV': JSON.stringify(process.env.NODE_ENV)
      }
    }),
    new webpack.optimize.OccurenceOrderPlugin(true),
    new webpack.optimize.UglifyJsPlugin({ minimize: true }),
    new HtmlWebpackPlugin({
      template: 'html/index.html',
      inject: 'body',
      filename: 'index.html'
    }),
    new webpack.NoErrorsPlugin()
  ],
  resolveLoader: {
    root: path.join(__dirname, 'node_modules')
  },
  resolve: {
    modulesDirectories: [
      'node_modules',
      'bower_components'
    ],
      extensions: ['', '.js', '.purs', '.css']
  }
};
