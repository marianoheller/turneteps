var subDays = require("date-fns/subDays");

exports.subDays = function (qty) {
  return function (date) {
    return subDays(date, qty);
  };
};
