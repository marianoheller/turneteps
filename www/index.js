const { main } = require("./main.js");

module.exports = (req, res) => {
  main();
  res.send("HELLO!");
};
