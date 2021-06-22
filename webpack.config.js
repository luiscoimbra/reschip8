const NodemonPlugin = require("nodemon-webpack-plugin");
const path = require("path");

module.exports = {
  entry: "./src/Reschip8.bs.js",
  mode: "development",
  output: {
    filename: "main.js",
    path: path.resolve(__dirname, "dist"),
  },
  target: "web",
  devServer: {
    port: 5000,
    open: true,
    watchContentBase: true,
  },
  plugins: [new NodemonPlugin()],
};
