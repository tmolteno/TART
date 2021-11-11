const FaviconsWebpackPlugin = require('favicons-webpack-plugin')

module.exports = {
  configureWebpack: {
    plugins: [
  	new FaviconsWebpackPlugin('./src/assets/tart_logo.svg'), // svg works too!
    ]
  },
  publicPath:
    process.env.NODE_ENV === "production"
      ? "/" +
        (typeof process.env.CI_PROJECT_NAME !== "undefined"
          ? process.env.CI_PROJECT_NAME + "/"
          : "")
      : "/",

}
