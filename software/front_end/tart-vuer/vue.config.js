
module.exports = {
  configureWebpack: {
    plugins: [
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
