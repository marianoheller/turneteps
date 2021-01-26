const { main } = require("./main.js");

module.exports = (req, res) => {
  main()
    .then((report) => {
      console.log("> Report: ", report);
      res.status(200).send(report);
    })
    .catch((err) => res.status(500).send(err.message || err));
};
