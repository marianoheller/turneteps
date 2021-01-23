var subDays = require("date-fns/subDays");
var addDays = require("date-fns/addDays");

exports.subDays = function (qty) {
  return function (date) {
    return subDays(date, qty);
  };
};

exports.addDays = function (qty) {
  return function (date) {
    return addDays(date, qty);
  };
};
