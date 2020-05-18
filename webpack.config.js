var path = require('path');

module.exports = {
  entry: './src/index.js',
  devServer: {
    contentBase: './dist',
  },
  output: {
    filename: 'main.js',
    path: path.resolve(__dirname, 'dist'),
  },
  module: {
    rules: [
      {
        test: /\.html$/,
        exclude: /node_modules/,
        loader: 'file-loader?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        use: [
          {
            loader: 'elm-hot-webpack-loader'
          },
          {
            loader: 'elm-webpack-loader',
            options: {
              cwd: __dirname
            }
          }
        ],
      }
    ]
  },

  devServer: {
    inline: true,
    stats: 'errors-only'
  }
}
